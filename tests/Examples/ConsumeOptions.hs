module Examples.ConsumeOptions where

import Options.Applicative
import qualified Options.Applicative.ConsumeA as ConsumeA

data Config = Config
  { outputFile :: Maybe FilePath
  , settings   :: [(String, String)]
  , verbose    :: Maybe ()
  } deriving Show

configParser :: Parser Config
configParser = Config
  <$> optional (consumeOption (ConsumeA.consumeOne "FILE" str)
      ( long "output"
     <> help "Output file path" ))
  <*> many (consumeOption (ConsumeA.consumePair "KEY" str "VALUE" str)
      ( long "set"
     <> help "Set a configuration key-value pair" ))
  <*> optional (consumeOption ConsumeA.consumeNone
      ( long "verbose"
     <> help "Enable verbose output" ))

opts :: ParserInfo Config
opts = info (configParser <**> helper)
  ( progDesc "Configure the application"
 <> header "consumeoptions - test for consume options" )
