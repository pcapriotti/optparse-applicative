import Control.Applicative
import Data.Default
import Options.Applicative
import Options.Applicative.Builder
import Options.Applicative.Extra

data Sample = Sample
  { hello :: String }

sample :: Parser Sample
sample = Sample
     <$> strOption
         ( long "hello"
         . metavar "TARGET"
         . help "Target for the greeting" )

greet :: Sample -> IO ()
greet (Sample h) = putStrLn $ "Hello, " ++ h

main :: IO ()
main = execParser opts (helper <*> sample) >>= greet
  where opts = def
             { execFullDesc = True
             , execProgDesc = "Print a greeting for TARGET"
             , execHeader = "hello - a test for optparse-applicative" }
