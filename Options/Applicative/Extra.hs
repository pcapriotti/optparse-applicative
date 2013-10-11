{-# LANGUAGE RankNTypes #-}
module Options.Applicative.Extra (
  -- * Extra parser utilities
  --
  -- | This module contains high-level functions to run parsers.
  helper,
  hsubparser,
  execParser,
  execParserPure,
  customExecParser,
  usage,
  ParserFailure(..),
  ) where

import Control.Applicative ((<$>), (<|>), (<**>))
import Data.Monoid (mconcat)
import System.Environment (getArgs, getProgName)
import System.Exit (exitWith, ExitCode(..))
import System.IO (hPutStr, stderr)

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
        ExitSuccess -> putStr msg
        _           -> hPutStr stderr msg
      exitWith c

data Result a = Result a
              | Extra ParserFailure

-- | A pure version 'execParser'.
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
  { errMessage = \progn
      -> with_context ctx pinfo $ \names ->
             return
           . show_help
           . add_error
           . add_usage names progn
  , errExitCode = exit_code }
  where
    add_usage names progn i = case msg of
      InfoMsg _ -> i
      _         -> i
        { infoHeader = vcat
            ( header_line i ++
              [ usage pprefs (infoParser i) ename ] ) }
      where
        ename = unwords (progn : names)
    add_error i = i
      { infoHeader = vcat (error_msg ++ [infoHeader i]) }
    error_msg = case msg of
      ShowHelpText -> []
      ErrorMsg m   -> [m]
      InfoMsg  m   -> [m]
    exit_code = case msg of
      InfoMsg _ -> ExitSuccess
      _         -> ExitFailure (infoFailureCode pinfo)
    show_full_help = case msg of
      ShowHelpText -> True
      _            -> prefShowHelpOnError pprefs
    show_help i
      | show_full_help
      = parserHelpText pprefs i
      | otherwise
      = unlines $ filter (not . null) [ infoHeader i ]
    header_line i
      | show_full_help
      = [ infoHeader i ]
      | otherwise
      = []

    with_context :: Context
                 -> ParserInfo a
                 -> (forall b . [String] -> ParserInfo b -> c)
                 -> c
    with_context NullContext i f = f [] i
    with_context (Context n i) _ f = f n i

-- | Generate option summary.
usage :: ParserPrefs -> Parser a -> String -> String
usage pprefs p progn = foldr (<+>) ""
  [ "Usage:"
  , progn
  , briefDesc pprefs p ]
