{-# LANGUAGE RankNTypes #-}
module Options.Applicative.Extra (
  -- * Extra parser utilities
  --
  -- | This module contains high-level functions to run parsers.
  helper,
  hsubparser,
  execParser,
  execParserMaybe,
  customExecParser,
  customExecParserMaybe,
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
import Data.Monoid
import Prelude
import System.Environment (getArgs, getProgName)
import System.Exit (exitSuccess, exitWith, ExitCode(..))
import System.IO (hPutStrLn, stderr)

import Options.Applicative.BashCompletion
import Options.Applicative.Builder hiding (briefDesc)
import Options.Applicative.Builder.Internal
import Options.Applicative.Common
import Options.Applicative.Help

import Options.Applicative.Internal
import Options.Applicative.Types

-- | A hidden \"helper\" option which always fails.
helper :: Parser (a -> a)
helper = abortOption ShowHelpText $ mconcat
  [ long "help"
  , short 'h'
  , help "Show this help text"
  , hidden ]

-- | Builder for a command parser with a \"helper\" option attached.
-- Used in the same way as `subparser`, but includes a \"--help|-h\" inside
-- the subcommand.
hsubparser :: Mod CommandFields a -> Parser a
hsubparser m = mkParser d g rdr
  where
    Mod _ d g = metavar "COMMAND" `mappend` m
    (groupName, cmds, subs) = mkCommand m
    rdr = CmdReader groupName cmds (fmap add_helper . subs)
    add_helper pinfo = pinfo
      { infoParser = infoParser pinfo <**> helper }

-- | Run a program description.
--
-- Parse command line arguments. Display help text and exit if any parse error
-- occurs.
execParser :: ParserInfo a -> IO a
execParser = customExecParser defaultPrefs

-- | Run a program description with custom preferences.
customExecParser :: ParserPrefs -> ParserInfo a -> IO a
customExecParser pprefs pinfo
  = execParserPure pprefs pinfo <$> getArgs
  >>= handleParseResult

-- | Handle `ParserResult`.
handleParseResult :: ParserResult a -> IO a
handleParseResult (Success a) = return a
handleParseResult (Failure failure) = do
      progn <- getProgName
      let (msg, exit) = renderFailure failure progn
      case exit of
        ExitSuccess -> putStrLn msg
        _           -> hPutStrLn stderr msg
      exitWith exit
handleParseResult (CompletionInvoked compl) = do
      progn <- getProgName
      msg <- execCompletion compl progn
      putStr msg
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

-- | Run a program description in pure code.
--
-- This function behaves like 'execParser', but can be called from pure code.
-- Note that, in case of errors, no message is displayed, and this function
-- simply returns 'Nothing'.
--
-- If you need to keep track of error messages, use 'execParserPure' instead.
{-# DEPRECATED execParserMaybe "Use execParserPure together with getParseResult instead" #-}
execParserMaybe :: ParserInfo a -> [String] -> Maybe a
execParserMaybe = customExecParserMaybe defaultPrefs

-- | Run a program description with custom preferences in pure code.
--
-- See 'execParserMaybe' for details.
{-# DEPRECATED customExecParserMaybe "Use execParserPure together with getParseResult instead" #-}
customExecParserMaybe :: ParserPrefs -> ParserInfo a -> [String] -> Maybe a
customExecParserMaybe pprefs pinfo args = getParseResult $ execParserPure pprefs pinfo args

-- | The most general way to run a program description in pure code.
execParserPure :: ParserPrefs       -- ^ Global preferences for this parser
               -> ParserInfo a      -- ^ Description of the program to run
               -> [String]          -- ^ Program arguments
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
-- @handleParseResult . Failure $ parserFailure pprefs pinfo ShowHelpText mempty@
parserFailure :: ParserPrefs -> ParserInfo a
              -> ParseError -> [Context]
              -> ParserFailure ParserHelp
parserFailure pprefs pinfo msg ctx = ParserFailure $ \progn ->
  let h = with_context ctx pinfo $ \names pinfo' -> mconcat
            [ base_help pinfo'
            , usage_help progn names pinfo'
            , error_help ]
  in (h, exit_code, prefColumns pprefs)
  where
    exit_code = case msg of
      ErrorMsg _       -> ExitFailure (infoFailureCode pinfo)
      UnknownError     -> ExitFailure (infoFailureCode pinfo)
      MissingError _ _ -> ExitFailure (infoFailureCode pinfo)
      ShowHelpText     -> ExitSuccess
      InfoMsg  _       -> ExitSuccess

    with_context :: [Context]
                 -> ParserInfo a
                 -> (forall b . [String] -> ParserInfo b -> c)
                 -> c
    with_context [] i f = f [] i
    with_context c@(Context _ i:_) _ f = f (contextNames c) i

    usage_help progn names i = case msg of
      InfoMsg _ -> mempty
      _         -> usageHelp $ vcatChunks
        [ pure . parserUsage pprefs (infoParser i) . unwords $ progn : names
        , fmap (indent 2) . infoProgDesc $ i ]

    error_help = errorHelp $ case msg of
      ShowHelpText                  -> mempty
      ErrorMsg m                    -> stringChunk m
      InfoMsg  m                    -> stringChunk m
      MissingError CmdStart _        | prefShowHelpOnEmpty pprefs
                                    -> mempty
      MissingError _ (SomeParser x) -> stringChunk "Missing:" <<+>> missingDesc pprefs x
      UnknownError                  -> mempty

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
      ShowHelpText             -> True
      MissingError CmdStart  _ | prefShowHelpOnEmpty pprefs
                               -> True
      _                        -> prefShowHelpOnError pprefs

renderFailure :: ParserFailure ParserHelp -> String -> (String, ExitCode)
renderFailure failure progn =
  let (h, exit, cols) = execFailure failure progn
  in (renderHelp cols h, exit)
