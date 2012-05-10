module Options.Applicative.Extra (
  -- * Extra parser utilities
  --
  -- | This module contains high-level functions to run parsers.
  helper,
  execParser,
  usage
  ) where

import Options.Applicative
import Options.Applicative.Builder
import Options.Applicative.Help
import Options.Applicative.Utils
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
  let p = infoParser pinfo
  case runParser p args of
    Just (a, []) -> return a
    _ -> do
      prog <- getProgName
      let pinfo' = pinfo
            { infoHeader = vcat [infoHeader pinfo, usage p prog] }
      hPutStr stderr $ parserHelpText pinfo'
      exitWith (ExitFailure (infoFailureCode pinfo))

-- | Generate option summary.
usage :: Parser a -> String -> String
usage p prog = foldr (<+>) ""
  [ "Usage:"
  , prog
  , briefDesc p ]
