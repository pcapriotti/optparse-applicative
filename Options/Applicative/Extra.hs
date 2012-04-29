module Options.Applicative.Extra where

import Options.Applicative
import Options.Applicative.Utils
import System.Environment
import System.Exit
import System.IO

execParser :: Parser a -> IO a
execParser p = do
  args <- getArgs
  case runParser p args of
    Just (a, []) -> return a
    _ -> usage p >> exitWith (ExitFailure 1)

usage :: Parser a -> IO ()
usage p = do
  prog <- getProgName
  hPutStrLn stderr (msg prog)
  where
    msg prog = foldr (<+>) ""
             [ "Usage:"
             , prog
             , shortDesc p ]
