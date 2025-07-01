{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
module Options.Applicative.Extra (
  -- * Extra parser utilities
  --
  -- | This module contains high-level functions to run parsers.
  helper,
  helperWith,
  hsubparser,
  simpleVersioner,
  execParser,
  customExecParser,
  execParserPure,
  getParseResult,
  handleParseResult,
  parserFailure,
  renderFailure,
  ParserFailure(..),
  overFailure,
  ParserResult(..),
  ParserPrefs(..),
  CompletionResult(..),
  ) where

import Control.Applicative
import Control.Monad (void)
import Data.Monoid
import Data.Foldable (traverse_)
import Prelude
import System.Environment (getProgName)
import System.Exit (exitSuccess, exitWith, ExitCode(..))
import System.IO (stderr)

import Options.Applicative.BashCompletion
import Options.Applicative.Builder
import Options.Applicative.Builder.Internal
import Options.Applicative.Common
import Options.Applicative.Help

import Options.Applicative.Internal
import Options.Applicative.Types
import qualified System.Process.Environment.OsString  as EOS
import System.OsPath (OsPath)
import "os-string" System.OsString (osstr, OsString)
import qualified "os-string" System.OsString as OsString
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.IO as Lazy
import qualified Data.Text.IO as Strict
import qualified Data.Text as Strict

-- | A hidden \"helper\" option which always fails.
--
-- A common usage pattern is to apply this applicatively when
-- creating a 'ParserInfo'
--
-- > opts :: ParserInfo Sample
-- > opts = info (sample <**> helper) mempty

helper :: Parser (a -> a)
helper =
  helperWith (mconcat [
    long [osstr|help|],
    short $ OsString.unsafeFromChar 'h',
    help "Show this help text"
  ])

-- | Like helper, but with a minimal set of modifiers that can be extended
-- as desired.
--
-- > opts :: ParserInfo Sample
-- > opts = info (sample <**> helperWith (mconcat [
-- >          long "help",
-- >          short 'h',
-- >          help "Show this help text",
-- >          hidden
-- >        ])) mempty
helperWith :: Mod OptionFields (a -> a) -> Parser (a -> a)
helperWith modifiers =
  option helpReader $
    mconcat
      [ value id,
        metavar Strict.empty,
        noGlobal,
        noArgError (ShowHelpText Nothing),
        hidden,
        modifiers
      ]
  where
    helpReader = do
      potentialCommand <- readerAsk
      readerAbort $
        ShowHelpText (Just potentialCommand)

-- | Builder for a command parser with a \"helper\" option attached.
-- Used in the same way as `subparser`, but includes a \"--help|-h\" inside
-- the subcommand.
hsubparser :: Mod CommandFields a -> Parser a
hsubparser m = mkParser d g rdr
  where
    Mod _ d g = metavar "COMMAND" `mappend` m
    (groupName, cmds) = mkCommand m
    rdr = CmdReader groupName ((fmap . fmap) add_helper cmds)
    add_helper pinfo = pinfo
      { infoParser = infoParser pinfo <**> helper }

-- | A hidden \"--version\" option that displays the version.
--
-- > opts :: ParserInfo Sample
-- > opts = info (sample <**> simpleVersioner "v1.2.3") mempty
simpleVersioner :: Strict.Text -- ^ Version string to be shown
                -> Parser (a -> a)
simpleVersioner version = infoOption version $
  mconcat
    [ long [osstr|version|]
    , help "Show version information"
    , hidden
    ]

-- | Run a program description.
--
-- Parse command line arguments. Display help text and exit if any parse error
-- occurs.
execParser :: ParserInfo a -> IO a
execParser = customExecParser defaultPrefs

-- | Run a program description with custom preferences.
customExecParser :: ParserPrefs -> ParserInfo a -> IO a
customExecParser pprefs pinfo
  = execParserPure pprefs pinfo <$> EOS.getArgs
  >>= handleParseResult

-- | Handle `ParserResult`.
handleParseResult :: ParserResult a -> IO a
handleParseResult (Success a) = return a
handleParseResult (Failure failure) = do
      -- TODO: OsString.getProgName (process/unix/Win32)?
      progn <- OsString.unsafeEncodeUtf <$> getProgName 
      let (msg, exit) = renderFailure failure progn
      case exit of
        ExitSuccess -> Lazy.putStrLn msg
        _           -> Lazy.hPutStrLn stderr msg
      exitWith exit
handleParseResult (CompletionInvoked compl) = do
      -- TODO: OsString.getProgName (process/unix/Win32)?
      progn <- OsString.unsafeEncodeUtf <$> getProgName
      msg <- execCompletion compl progn
      Strict.putStr msg
      exitSuccess

-- | Extract the actual result from a `ParserResult` value.
--
-- This function returns 'Nothing' in case of errors.  Possible error messages
-- or completion actions are simply discarded.
--
-- If you want to display error messages and invoke completion actions
-- appropriately, use 'handleParseResult' instead.
getParseResult :: ParserResult a -> Maybe a
getParseResult (Success a) = Just a
getParseResult _ = Nothing

-- | The most general way to run a program description in pure code.
execParserPure :: ParserPrefs       -- ^ Global preferences for this parser
               -> ParserInfo a      -- ^ Description of the program to run
               -> [OsPath]          -- ^ Program arguments
               -> ParserResult a
execParserPure pprefs pinfo args =
  case runP p pprefs of
    (Right (Right r), _) -> Success r
    (Right (Left c), _) -> CompletionInvoked c
    (Left err, ctx) -> Failure $ parserFailure pprefs pinfo err ctx
  where
    pinfo' = pinfo
      { infoParser = (Left <$> bashCompletionParser pinfo pprefs)
                 <|> (Right <$> infoParser pinfo) }
    p = runParserInfo pinfo' args

-- | Generate a `ParserFailure` from a `ParseError` in a given `Context`.
--
-- This function can be used, for example, to show the help text for a parser:
--
-- @handleParseResult . Failure $ parserFailure pprefs pinfo (ShowHelpText Nothing) mempty@
parserFailure :: ParserPrefs -> ParserInfo a
              -> ParseError -> [Context]
              -> ParserFailure ParserHelp
parserFailure pprefs pinfo msg ctx0 = ParserFailure $ \progn ->
  let h = with_context ctx pinfo $ \names pinfo' -> mconcat
            [ base_help pinfo'
            , usage_help progn names pinfo'
            , suggestion_help
            , globals ctx
            , error_help ]
  in (h, exit_code, prefColumns pprefs)
  where
    --
    -- Add another context layer if the argument to --help is
    -- a valid command.
    ctx = case msg of
      ShowHelpText (Just potentialCommand) ->
        let ctx1 = with_context ctx0 pinfo $ \_ pinfo' ->
              snd
                $ flip runP defaultPrefs { prefBacktrack = SubparserInline }
                $ runParserStep (infoPolicy pinfo') (infoParser pinfo') potentialCommand []
        in ctx1 `mappend` ctx0
      _ ->
        ctx0

    exit_code = case msg of
      ErrorMsg {}        -> ExitFailure (infoFailureCode pinfo)
      UnknownError       -> ExitFailure (infoFailureCode pinfo)
      MissingError {}    -> ExitFailure (infoFailureCode pinfo)
      ExpectsArgError {} -> ExitFailure (infoFailureCode pinfo)
      UnexpectedError {} -> ExitFailure (infoFailureCode pinfo)
      ShowHelpText {}    -> ExitSuccess
      InfoMsg {}         -> ExitSuccess

    with_context :: [Context]
                 -> ParserInfo a
                 -> (forall b . [OsString] -> ParserInfo b -> c)
                 -> c
    with_context [] i f = f [] i
    with_context c@(Context _ i:_) _ f = f (contextNames c) i

    globals :: [Context] -> ParserHelp
    globals cs =
      let
        voided =
          fmap (\(Context _ p) -> void p) cs `mappend` pure (void pinfo)

        globalParsers =
          traverse_ infoParser $
            drop 1 voided
      in
        if prefHelpShowGlobal pprefs then
          parserGlobals pprefs globalParsers
        else
          mempty

    usage_help progn names i = case msg of
      InfoMsg _
        -> mempty
      _
        -> mconcat [
            usageHelp (pure . parserUsage pprefs (infoParser i) . (OsString.intercalate [osstr| |]) $ progn : names)
          , descriptionHelp (infoProgDesc i)
          ]

    error_help = errorHelp $ case msg of
      ShowHelpText {}
        -> mempty

      ErrorMsg m
        -> stringChunk m

      InfoMsg  m
        -> stringChunk m

      MissingError CmdStart _
        | prefShowHelpOnEmpty pprefs
        -> mempty

      MissingError _ (SomeParser x)
        -> stringChunk "Missing:" <<+>> missingDesc pprefs x

      ExpectsArgError x
        -> stringChunk $ "The option `" <> osStringToStrictText x <> "` expects an argument."

      UnexpectedError arg _
        -> stringChunk msg'
          where
            arg' = osStringToStrictText arg
            -- This gives us the same error we have always
            -- reported
            msg' = case OsString.uncons arg of
              Just (char,_) | OsString.unsafeFromChar '-' == char -> "Invalid option `" <> arg' <> "'"
              _       -> "Invalid argument `" <> arg' <> "'"

      UnknownError
        -> mempty


    suggestion_help = suggestionsHelp $ case msg of
      UnexpectedError arg (SomeParser x)
        --
        -- We have an unexpected argument and the parser which
        -- it's running over.
        --
        -- We can make a good help suggestion here if we do
        -- a levenstein distance between all possible suggestions
        -- and the supplied option or argument.
        -> suggestions
          where
            --
            -- Not using chunked here, as we don't want to
            -- show "Did you mean" if there's nothing there
            -- to show
            suggestions = (.$.) <$> prose
                                <*> (indent 4 <$> (vcatChunks . fmap (stringChunk . osStringToStrictText) $ good ))

            --
            -- We won't worry about the 0 case, it won't be
            -- shown anyway.
            prose       = if length good < 2 then
                            stringChunk "Did you mean this?"
                          else
                            stringChunk "Did you mean one of these?"
            --
            -- Suggestions we will show, they're close enough
            -- to what the user wrote
            good        = filter isClose possibles

            --
            -- Bit of an arbitrary decision here.
            -- Edit distances of  or 2 will give hints
            isClose :: OsString -> Bool
            isClose a   = editDistance (unsafePerformIO . OsString.decodeLE $ a) (unsafePerformIO . OsString.decodeLE $ arg) < 3

            --
            -- Similar to how bash completion works.
            -- We map over the parser and get the names
            -- ( no IO here though, unlike for completers )
            possibles   = concat $ mapParser opt_completions x

            --
            -- Look at the option and give back the possible
            -- things the user could type. If it's a command
            -- reader also ensure that it can be immediately
            -- reachable from where the error was given.
            opt_completions reachability opt = case optMain opt of
              OptReader ns _ _ -> fmap showOption ns
              FlagReader ns _  -> fmap showOption ns
              ArgReader _      -> []
              CmdReader _ ns    | argumentIsUnreachable reachability
                               -> []
                                | otherwise
                               -> fst <$> ns
      _
        -> mempty

    base_help :: ParserInfo a -> ParserHelp
    base_help i
      | show_full_help
      = mconcat [h, f, parserHelp pprefs (infoParser i)]
      | otherwise
      = mempty
      where
        h = headerHelp (infoHeader i)
        f = footerHelp (infoFooter i)

    show_full_help = case msg of
      ShowHelpText {}          -> True
      MissingError CmdStart  _  | prefShowHelpOnEmpty pprefs
                               -> True
      InfoMsg _                -> False
      _                        -> prefShowHelpOnError pprefs

renderFailure :: ParserFailure ParserHelp -> OsString -> (Lazy.Text, ExitCode)
renderFailure failure progn =
  let (h, exit, cols) = execFailure failure progn
  in (renderHelp cols h, exit)
