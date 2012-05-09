module Options.Applicative.Extra where

import Options.Applicative
import Options.Applicative.Types
import Options.Applicative.Builder
import Options.Applicative.Help
import Options.Applicative.Utils
import System.Environment
import System.Exit
import System.IO

helper :: Parser (a -> a)
helper = nullOption
       ( long "help"
       & short 'h'
       & help "Show this help text"
       & value id
       & hide )

execParser :: ParserInfo a -> IO a
execParser info = do
  args <- getArgs
  let p = infoParser info
  case runParser p args of
    Just (a, []) -> return a
    _ -> do
      prog <- getProgName
      let info' = info
            { infoHeader = vcat [infoHeader info, usage p prog] }
      hPutStr stderr $ parserHelpText info'
      exitWith (ExitFailure 1)

usage :: Parser a -> String -> String
usage p prog = foldr (<+>) ""
  [ "Usage:"
  , prog
  , shortDesc p ]
