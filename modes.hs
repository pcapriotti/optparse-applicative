import Control.Applicative
import Options.Applicative
import Options.Applicative.Builder
import System.Environment
import System.IO

data Sample
  = Hello String
  | Goodbye

hello :: Parser Sample
hello = Hello <$> strOption (long "whom" . short 'w')

sample :: Parser Sample
sample = command (`lookup`
  [("hello", Hello <$> strOption (long "whom" . short 'w' . value "world"))
  ,("goodbye", pure Goodbye)])
  id

main :: IO ()
main = do
  args <- getArgs
  case runParser sample args of
    Nothing -> hPutStrLn stderr "Parse error"
    Just (s, _) -> case s of
      Hello w -> putStrLn $ "Hello, " ++ w ++ "!"
      Goodbye -> putStrLn "Goodbye."
