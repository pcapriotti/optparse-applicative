import Options.Applicative

data Sample
  = Hello String
  | Goodbye

hello :: Parser Sample
hello = Hello <$> argument str (metavar "TARGET")

sample :: Parser Sample
sample = subparser
       ( command "hello"
         (info hello
               (progDesc "Print greeting"))
       & command "goodbye"
         (info (pure Goodbye)
               (progDesc "Say goodbye"))
       )

run :: Sample -> IO ()
run (Hello target) = putStrLn $ "Hello, " ++ target ++ "!"
run Goodbye = putStrLn "Goodbye."

main :: IO ()
main = execParser (info sample idm) >>= run
