import Control.Applicative
import Options.Applicative
import Options.Applicative.Builder
import Options.Applicative.Extra

data Sample
  = Hello String
  | Goodbye

hello :: Parser Sample
hello = Hello <$> argument str (metavar "TARGET")

sample :: Parser Sample
sample = subparser
       ( command "hello" (info hello)
           { infoProgDesc = "Print greeting" }
       & command "goodbye" (info (pure Goodbye))
           { infoProgDesc = "Say goodbye" }
       )

run :: Sample -> IO ()
run (Hello target) = putStrLn $ "Hello, " ++ target ++ "!"
run Goodbye = putStrLn "Goodbye."

main :: IO ()
main = execParser (info sample) >>= run
