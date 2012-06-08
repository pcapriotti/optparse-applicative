module Options.Applicative.Extra (
  -- * Extra parser utilities
  --
  -- | This module contains high-level functions to run parsers.
  helper,
  execParser,
  usage
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

-- | Run a program description.
--
-- Parse command line arguments. Display help text and exit if any parse error
-- occurs.
execParser :: ParserInfo a -> IO a
execParser pinfo = do
  args <- getArgs
  let p = pinfo^.infoParser
  case runParser p args of
    Just (a, []) -> return a
    _ -> do
      prog <- getProgName
      let add_usage = modL infoHeader $ \h -> vcat [h, usage p prog]
      hPutStr stderr $ parserHelpText (add_usage pinfo)
      exitWith (ExitFailure (pinfo^.infoFailureCode))

-- | Generate option summary.
usage :: Parser a -> String -> String
usage p prog = foldr (<+>) ""
  [ "Usage:"
  , prog
  , briefDesc p ]
