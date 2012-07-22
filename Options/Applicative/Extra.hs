{-# LANGUAGE RankNTypes #-}
module Options.Applicative.Extra (
  -- * Extra parser utilities
  --
  -- | This module contains high-level functions to run parsers.
  helper,
  execParser,
  execParserPure,
  usage,
  ParserFailure(..),
  ) where

import Data.Lens.Common
import Options.Applicative.Common
import Options.Applicative.Builder
import Options.Applicative.Help
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
      hPutStr stderr (errMessage failure progn)
      exitWith (errExitCode failure)

-- | A pure version 'execParser'.
execParserPure :: ParserPrefs       -- ^ Global preferences for this parser
               -> ParserInfo a      -- ^ Description of the program to run
               -> [String]          -- ^ Program arguments
               -> Either ParserFailure a
execParserPure pprefs pinfo args =
  case runP p of
    (Right a, _) -> Right a
    (Left msg, ctx) -> Left ParserFailure
      { errMessage = \progn
          -> with_context ctx pinfo $ \name ->
                 parserHelpText pprefs
               . add_error msg
               . add_usage name progn
      , errExitCode = ExitFailure (pinfo^.infoFailureCode) }
  where
    parser = pinfo^.infoParser
    add_usage name progn i =
      modL infoHeader
           (\h -> vcat [h, usage pprefs (i^.infoParser) ename])
           i
      where
        ename = maybe progn (\n -> progn ++ " " ++ n) name
    add_error msg = modL infoHeader $ \h -> vcat [msg, h]

    with_context :: Context
                 -> ParserInfo a
                 -> (forall b . Maybe String -> ParserInfo b -> c)
                 -> c
    with_context NullContext i f = f Nothing i
    with_context (Context n i) _ f = f n i

    p = runParserFully parser args

-- | Generate option summary.
usage :: ParserPrefs -> Parser a -> String -> String
usage pprefs p progn = foldr (<+>) ""
  [ "Usage:"
  , progn
  , briefDesc pprefs p ]
