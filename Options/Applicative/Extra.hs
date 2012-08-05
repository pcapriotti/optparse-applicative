{-# LANGUAGE RankNTypes #-}
module Options.Applicative.Extra (
  -- * Extra parser utilities
  --
  -- | This module contains high-level functions to run parsers.
  helper,
  execParser,
  execParserPure,
  customExecParser,
  usage,
  ParserFailure(..),
  ) where

import Control.Applicative
import Options.Applicative.BashCompletion
import Options.Applicative.Common
import Options.Applicative.Builder
import Options.Applicative.Help
import Options.Applicative.Internal
import Options.Applicative.Utils
import Options.Applicative.Types
import System.Environment
import System.Exit
import System.IO

-- | A hidden \"helper\" option which always fails.
helper :: Parser (a -> a)
helper = nullOption
       ( long "help"
       & short 'h'
       & help "Show this help text"
       & value id
       & metavar ""
       & hidden )

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
    (Left msg, ctx) -> Left ParserFailure
      { errMessage = \progn
          -> with_context ctx pinfo $ \names ->
                 return
               . parserHelpText pprefs
               . add_error msg
               . add_usage names progn
      , errExitCode = ExitFailure (infoFailureCode pinfo) }
  where
    parser = infoParser pinfo
    add_usage names progn i = i
      { infoHeader = vcat
          [ infoHeader i
          , usage pprefs (infoParser i) ename ] }
      where
        ename = unwords (progn : names)
    add_error msg i = i
      { infoHeader = vcat [msg, infoHeader i] }

    with_context :: Context
                 -> ParserInfo a
                 -> (forall b . [String] -> ParserInfo b -> c)
                 -> c
    with_context NullContext i f = f [] i
    with_context (Context n i) _ f = f n i

    parser' = (Extra <$> bashCompletionParser parser pprefs)
          <|> (Result <$> parser)
    p = runParserFully parser' args

-- | Generate option summary.
usage :: ParserPrefs -> Parser a -> String -> String
usage pprefs p progn = foldr (<+>) ""
  [ "Usage:"
  , progn
  , briefDesc pprefs p ]
