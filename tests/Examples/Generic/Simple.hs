{-# LANGUAGE DeriveGeneric #-}
module Examples.Generic.Simple where

import GHC.Generics
import Options.Applicative
import Options.Applicative.Generic


data Sample = Sample
  { hello  :: String
  , quiet  :: Bool
  , repeat :: Int }
  deriving (Show, Generic)

opts :: ParserInfo Sample
opts = info (simpleOptions <**> helper)
  ( fullDesc
  <> progDesc "Print a greeting"
  <> header "generic_simple - a test for optparse-applicative" )
