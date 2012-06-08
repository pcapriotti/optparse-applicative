module Examples.Hello where

import Options.Applicative

data Sample = Sample
  { hello :: String
  , quiet :: Bool }
  deriving Show

sample :: Parser Sample
sample = Sample
     <$> strOption
         ( long "hello"
         & metavar "TARGET"
         & help "Target for the greeting" )
     <*> switch
         ( long "quiet"
         & help "Whether to be quiet" )

greet :: Sample -> IO ()
greet (Sample h True) = putStrLn $ "Hello, " ++ h
greet _ = return ()

main :: IO ()
main = execParser opts >>= greet

opts :: ParserInfo Sample
opts = info (helper <*> sample)
  ( fullDesc
  & progDesc "Print a greeting for TARGET"
  & header "hello - a test for optparse-applicative" )
