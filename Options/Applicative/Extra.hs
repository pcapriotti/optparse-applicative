module Options.Applicative.Extra where

import Control.Monad
import Data.Default
import Options.Applicative
import Options.Applicative.Utils
import System.Environment
import System.Exit
import System.IO

data ExecOptions = ExecOptions
  { execFullDesc :: Bool
  , execHeader :: String
  , execProgDesc :: String
  , execFooter :: String
  , execFailureCode :: Int }

instance Default ExecOptions where
 def = ExecOptions
     { execFullDesc = False
     , execHeader = ""
     , execProgDesc = ""
     , execFooter = ""
     , execFailureCode = 1 }

execParser :: ExecOptions -> Parser a -> IO a
execParser opts p = do
  args <- getArgs
  case runParser p args of
    Just (a, []) -> return a
    _ -> do
      unless (null (execHeader opts)) $
        putStrLn $ execHeader opts ++ "\n"
      usage p
      unless (null (execProgDesc opts)) $
        putStrLn $ "  " ++ execProgDesc opts
      when (execFullDesc opts) $ do
        putStrLn "\nCommon options:"
        putStrLn $ fullDesc p
      unless (null (execFooter opts)) $
        putStrLn $ '\n' : execFooter opts
      exitWith (ExitFailure 1)

usage :: Parser a -> IO ()
usage p = do
  prog <- getProgName
  hPutStrLn stderr (msg prog)
  where
    msg prog = foldr (<+>) ""
             [ "Usage:"
             , prog
             , shortDesc p ]
