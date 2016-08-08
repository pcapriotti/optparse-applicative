{-# LANGUAGE CPP #-}
module Examples.Hello where

import Data.Monoid
import Options.Applicative

#if __GLASGOW_HASKELL__ <= 702
(<>) :: Monoid a => a -> a -> a
(<>) = mappend
#endif

data Sample = Sample
  { hello :: String
  , quiet :: Bool }
  deriving Show

sample :: Parser Sample
sample = Sample
     <$> strOption
         ( long "hello"
        <> metavar "TARGET"
        <> help "Target for the greeting" )
     <*> switch
         ( long "quiet"
        <> help "Whether to be quiet" )

greet :: Sample -> IO ()
greet (Sample h False) = putStrLn $ "Hello, " ++ h
greet _ = return ()

main :: IO ()
main = execParser opts >>= greet

opts :: ParserInfo Sample
opts = info (sample <**> helper)
  ( fullDesc
 <> progDesc "Print a greeting for TARGET"
 <> header "hello - a test for optparse-applicative" )
