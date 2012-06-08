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
       & hide )

-- | Result after a parse error.
data ParserFailure = ParserFailure
  { errMessage :: String -> String -- ^ Function which takes the program name
                                   -- as input and returns an error message
  , errExitCode :: ExitCode        -- ^ Exit code to use for this error
  }

-- | Run a program description.
--
-- Parse command line arguments. Display help text and exit if any parse error
-- occurs.
execParser :: ParserInfo a -> IO a
execParser pinfo = do
  args <- getArgs
  case execParserPure pinfo args of
    Right a -> return a
    Left failure -> do
      progn <- getProgName
      hPutStr stderr (errMessage failure progn)
      exitWith (errExitCode failure)

-- | A pure version 'execParser'.
execParserPure :: ParserInfo a      -- ^ Description of the program to run
               -> [String]          -- ^ Program arguments
               -> Either ParserFailure a
execParserPure pinfo args =
  case runParser parser args of
    Just (a, []) -> Right a
    _ -> Left ParserFailure
      { errMessage = \progn -> parserHelpText (add_usage progn pinfo)
      , errExitCode = ExitFailure (pinfo^.infoFailureCode) }
  where
    parser = pinfo^.infoParser
    add_usage progn = modL infoHeader $ \h -> vcat [h, usage parser progn]


-- | Generate option summary.
usage :: Parser a -> String -> String
usage p progn = foldr (<+>) ""
  [ "Usage:"
  , progn
  , briefDesc p ]