{-# LANGUAGE CPP #-}
module Examples.ConsumeOptions where

import Options.Applicative
import qualified Options.Applicative.ConsumeA as ConsumeA
#if __GLASGOW_HASKELL__ < 804
import Data.Semigroup hiding (Option, option)
#endif

data Config = Config
  { outputFile :: Maybe FilePath
  , settings   :: [(String, String)]
  , verbose    :: Maybe ()
  } deriving Show

configParser :: Parser Config
configParser = Config
  <$> optional (consumeOption (ConsumeA.consumeOne str (metavar "FILE"))
      ( long "output"
     <> help "Output file path" ))
  <*> many (consumeOption (ConsumeA.consumePair str (metavar "KEY") str (metavar "VALUE"))
      ( long "set"
     <> help "Set a configuration key-value pair" ))
  <*> optional (consumeOption ConsumeA.consumeNone
      ( long "verbose"
     <> help "Enable verbose output" ))

opts :: ParserInfo Config
opts = info (configParser <**> helper)
  ( progDesc "Configure the application"
 <> header "consumeoptions - test for consume options" )
