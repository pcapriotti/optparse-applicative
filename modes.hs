import Control.Applicative
import Data.Default
import Options.Applicative
import Options.Applicative.Builder
import Options.Applicative.Extra

data Sample
  = Hello String
  | Goodbye

hello :: Parser Sample
hello = Hello <$> argument str (metavar "TARGET")

sample :: Parser Sample
sample = command (`lookup`
  [("hello", hello)
  ,("goodbye", pure Goodbye)])
  id

run :: Sample -> IO ()
run (Hello target) = putStrLn $ "Hello, " ++ target ++ "!"
run Goodbye = putStrLn "Goodbye."

main :: IO ()
main = execParser def sample >>= run
