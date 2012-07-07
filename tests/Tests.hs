{-# LANGUAGE TemplateHaskell #-}
module Main where

import qualified Examples.Hello as Hello
import qualified Examples.Commands as Commands
import qualified Examples.Cabal as Cabal

import Options.Applicative.Extra
import Options.Applicative.Types
import System.Exit
import Test.HUnit
import Test.Framework.Providers.HUnit
import Test.Framework.TH.Prime

assertLeft :: Show b => Either a b -> (a -> Assertion) -> Assertion
assertLeft x f = either f err x
  where
    err b = assertFailure $ "expected Left, got " ++ show b

checkHelpText :: Show a => String -> ParserInfo a -> [String] -> Assertion
checkHelpText name p args = do
  let result = execParserPure p args
  assertLeft result $ \(ParserFailure err code) -> do
    expected <- readFile $ "tests/" ++ name ++ ".err.txt"
    expected @=? err name
    ExitFailure 1 @=? code

case_hello :: Assertion
case_hello = checkHelpText "hello" Hello.opts ["--help"]

case_modes :: Assertion
case_modes = checkHelpText "commands" Commands.opts ["--help"]

case_cabal :: Assertion
case_cabal = checkHelpText "cabal" Cabal.pinfo ["configure", "--help"]

main :: IO ()
main = $(defaultMainGenerator)
