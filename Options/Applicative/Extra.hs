module Options.Applicative.Extra where

import Control.Monad
import Data.Default
import Options.Applicative
import Options.Applicative.Types
import Options.Applicative.Builder
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
     { execFullDesc = True
     , execHeader = ""
     , execProgDesc = ""
     , execFooter = ""
     , execFailureCode = 1 }

helper :: Parser (a -> a)
helper = nullOption
       ( long "help"
       & short 'h'
       & help "Show this help text"
       & value id
       & hide )

execParser :: ExecOptions -> Parser a -> IO a
execParser opts p = do
  args <- getArgs
  case runParser p args of
    Just (a, []) -> return a
    _ -> do
      unless (null (execHeader opts)) $
        putStrLn $ execHeader opts ++ "\n"
      prog <- getProgName
      usage p prog
      unless (null (execProgDesc opts)) $
        putStrLn $ "  " ++ execProgDesc opts
      let desc = fullDesc p
      when (execFullDesc opts && not (null desc)) $ do
        putStrLn "\nCommon options:"
        putStrLn desc
      unless (null (execFooter opts)) $
        putStrLn $ '\n' : execFooter opts
      exitWith (ExitFailure 1)

usage :: Parser a -> String -> IO ()
usage p prog = hPutStrLn stderr msg
  where
    msg = foldr (<+>) ""
        [ "Usage:"
        , prog
        , shortDesc p ]
