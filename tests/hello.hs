import Control.Applicative
import Options.Applicative
import Options.Applicative.Builder
import Options.Applicative.Extra

data Sample = Sample
  { hello :: String }

sample :: Parser Sample
sample = Sample
     <$> strOption
         ( long "hello"
         & metavar "TARGET"
         & help "Target for the greeting" )

greet :: Sample -> IO ()
greet (Sample h) = putStrLn $ "Hello, " ++ h

main :: IO ()
main = execParser opts >>= greet
  where
    opts = (info $ helper <*> sample)
      { infoFullDesc = True
      , infoProgDesc = "Print a greeting for TARGET"
      , infoHeader = "hello - a test for optparse-applicative" }
