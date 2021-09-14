{-# LANGUAGE DeriveGeneric #-}
module Examples.Generic.Commands where

import GHC.Generics
import Options.Applicative
import Options.Applicative.Generic


data Sample
    = Cat String
    | Copy
        { file :: String
        , target :: String          
        }
  deriving (Show, Generic)

opts :: ParserInfo Sample
opts = info (simpleCommands <**> helper)
  ( fullDesc
  <> progDesc "Do multiple things"
  <> header "generic_commands - a test for optparse-applicative" )
