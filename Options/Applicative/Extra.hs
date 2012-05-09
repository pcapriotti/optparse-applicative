module Options.Applicative.Extra (
  helper,
  execParser,
  usage
  ) where

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
      exitWith (ExitFailure 1)

usage :: Parser a -> String -> String
usage p prog = foldr (<+>) ""
  [ "Usage:"
  , prog
  , shortDesc p ]
