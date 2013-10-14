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
  usage,
  ParserFailure(..),
  ) where

import Control.Applicative (pure, (<$>), (<|>), (<**>))
import Data.Monoid (mempty, mconcat)
import System.Environment (getArgs, getProgName)
import System.Exit (exitWith, ExitCode(..))
import System.IO (hPutStrLn, stderr)

import Options.Applicative.BashCompletion
import Options.Applicative.Builder hiding (briefDesc)
import Options.Applicative.Builder.Internal
import Options.Applicative.Common
import Options.Applicative.Help
import Options.Applicative.Internal
import Options.Applicative.Types
import Options.Applicative.Utils

-- | A hidden \"helper\" option which always fails.
helper :: Parser (a -> a)
helper = abortOption ShowHelpText $ mconcat
  [ long "help"
  , short 'h'
  , help "Show this help text" ]

hsubparser :: Mod CommandFields a -> Parser a
hsubparser m = mkParser d g rdr
  where
    Mod _ d g = m `mappend` metavar "COMMAND"
    (cmds, subs) = mkCommand m
    rdr = CmdReader cmds (fmap add_helper . subs)
    add_helper pinfo = pinfo
      { infoParser = infoParser pinfo <**> helper }

-- | Run a program description.
--
-- Parse command line arguments. Display help text and exit if any parse error
-- occurs.
execParser :: ParserInfo a -> IO a
execParser = customExecParser (prefs idm)

-- | Run a program description with custom preferences.
customExecParser :: ParserPrefs -> ParserInfo a -> IO a
customExecParser pprefs pinfo = do
  args <- getArgs
  case execParserPure pprefs pinfo args of
    Right a -> return a
    Left failure -> do
      progn <- getProgName
      let c = errExitCode failure
      msg <- errMessage failure progn
      case c of
        ExitSuccess -> putStrLn msg
        _           -> hPutStrLn stderr msg
      exitWith c

-- | Run a program description in pure code.
--
-- This function behaves like 'execParser', but can be called from pure code.
-- Note that, in case of errors, no message is displayed, and this function
-- simply returns 'Nothing'.
--
-- If you need to keep track of error messages, use 'execParserPure' instead.
execParserMaybe :: ParserInfo a -> [String] -> Maybe a
execParserMaybe = customExecParserMaybe (prefs idm)

-- | Run a program description with custom preferences in pure code.
--
-- See 'execParserMaybe' for details.
customExecParserMaybe :: ParserPrefs -> ParserInfo a -> [String] -> Maybe a
customExecParserMaybe pprefs pinfo
  = either (const Nothing) Just
  . execParserPure pprefs pinfo

data Result a = Result a
              | Extra ParserFailure

-- | The most general way to run a program description in pure code.
execParserPure :: ParserPrefs       -- ^ Global preferences for this parser
               -> ParserInfo a      -- ^ Description of the program to run
               -> [String]          -- ^ Program arguments
               -> Either ParserFailure a
execParserPure pprefs pinfo args =
  case runP p pprefs of
    (Right r, _) -> case r of
      Result a -> Right a
      Extra failure -> Left failure
    (Left msg, ctx) -> Left $
      parserFailure pprefs pinfo msg ctx
  where
    parser = infoParser pinfo
    parser' = (Extra <$> bashCompletionParser parser pprefs)
          <|> (Result <$> parser)
    p = runParserFully parser' args

parserFailure :: ParserPrefs -> ParserInfo a
              -> ParseError -> Context
              -> ParserFailure
parserFailure pprefs pinfo msg ctx = ParserFailure
--  { errMessage = \progn
--      -> with_context ctx pinfo $ \names ->
--             return
--           . show_help
--           . add_error
--           . add_usage names progn
--  , errExitCode = exit_code }
--  where
--    add_usage names progn i = case msg of
--      InfoMsg _ -> i
--      _         -> i
--        { infoHeader = vcat
--            ( header_line i ++
--              [ usage pprefs (infoParser i) ename ] ) }
--      where
--        ename = unwords (progn : names)
--    add_error i = i
--      { infoHeader = vcat (error_msg ++ [infoHeader i]) }
--    error_msg = case msg of
--      ShowHelpText -> []
--      ErrorMsg m   -> [m]
--      InfoMsg  m   -> [m]
--    exit_code = case msg of
--      InfoMsg _ -> ExitSuccess
--      _         -> ExitFailure (infoFailureCode pinfo)
--    show_full_help = case msg of
--      ShowHelpText -> True
--      _            -> prefShowHelpOnError pprefs
--    show_help i
--      | show_full_help
--      = parserHelpText pprefs i
--      | otherwise
--      = unlines $ filter (not . null) [ infoHeader i ]
--    header_line i
--      | show_full_help
--      = [ infoHeader i ]
--      | otherwise
--      = []

  { errMessage = \progn -> do
      let h = with_context ctx pinfo $ \names pinfo' -> mconcat
                [ base_help names pinfo'
                , usage_help progn names pinfo'
                , error_help ]
      return . render_help $ h
  , errExitCode = case msg of
      InfoMsg _ -> ExitSuccess
      _         -> ExitFailure (infoFailureCode pinfo) }
  where
    with_context :: Context
                 -> ParserInfo a
                 -> (forall b . [String] -> ParserInfo b -> c)
                 -> c
    with_context NullContext i f = f [] i
    with_context (Context n i) _ f = f n i

    render_help :: ParserHelp -> String
    render_help = (`displayS` "") . renderPretty 1.0 80 . helpText

    show_full_help = case msg of
      ShowHelpText -> True
      _            -> prefShowHelpOnError pprefs

    usage_help progn names i = case msg of
      InfoMsg _ -> mempty
      _         -> usageHelp $ vcatChunks
        [ pure . usage pprefs (infoParser i) . unwords $ progn : names
        , fmap (indent 2) . stringChunk . infoProgDesc $ pinfo ]

    error_help = headerHelp $ case msg of
      ShowHelpText -> mempty
      ErrorMsg m   -> stringChunk m
      InfoMsg  m   -> stringChunk m

    base_help :: [String] -> ParserInfo a -> ParserHelp
    base_help names pinfo
      | show_full_help
      = parserHelp pprefs $ pinfo
      | otherwise
      = headerHelp (stringChunk (infoHeader pinfo))

-- parserFailure :: ParserPrefs -> ParserInfo a
--               -> ParseError -> Context
--               -> ParserFailure
-- parserFailure pprefs pinfo msg ctx = ParserFailure
--   { errMessage = \progn
--       -> with_context ctx pinfo $ \names ->
--              return
--            . show_help
--            . add_error
--            . add_usage names progn
--   , errExitCode = exit_code }
--   where
--     add_usage names progn i = case msg of
--       InfoMsg _ -> i
--       _         -> i
--         { infoHeader = vcat
--             ( header_line i ++
--               [ usage pprefs (infoParser i) ename ] ) }
--       where
--         ename = unwords (progn : names)
--     add_error i = i
--       { infoHeader = vcat (error_msg ++ [infoHeader i]) }
--     error_msg = case msg of
--       ShowHelpText -> []
--       ErrorMsg m   -> [m]
--       InfoMsg  m   -> [m]
--     exit_code = case msg of
--       InfoMsg _ -> ExitSuccess
--       _         -> ExitFailure (infoFailureCode pinfo)
--     show_full_help = case msg of
--       ShowHelpText -> True
--       _            -> prefShowHelpOnError pprefs
--     show_help i
--       | show_full_help
--       = parserHelpText pprefs i
--       | otherwise
--       = unlines $ filter (not . null) [ infoHeader i ]
--     header_line i
--       | show_full_help
--       = stringChunk (infoHeader i)
--       | otherwise
--       = mempty
-- 
--     with_context :: Context
--                  -> ParserInfo a
--                  -> (forall b . [String] -> ParserInfo b -> c)
--                  -> c
--     with_context NullContext i f = f [] i
--     with_context (Context n i) _ f = f n i

-- | Generate option summary.
usage :: ParserPrefs -> Parser a -> String -> Doc
usage pprefs p progn = hsep $
  [ string "Usage:"
  , string progn
  , extract (briefDesc pprefs p) ]
