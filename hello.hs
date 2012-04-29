import Control.Applicative
import Options.Applicative
import Options.Applicative.Builder
import System.Environment
import System.IO

data Sample = Sample
  { hello :: String }

sample :: Parser Sample
sample = Sample <$> option "hello" (this (reader str))

greet :: Sample -> IO ()
greet (Sample h) = putStrLn $ "Hello, " ++ h

main :: IO ()
main = do
  args <- getArgs
  case runParser sample args of
    Nothing -> hPutStrLn stderr "Parse error"
    Just (s, _) -> greet s
