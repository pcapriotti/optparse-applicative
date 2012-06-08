{-# LANGUAGE TemplateHaskell #-}
module Main where

import qualified Examples.Hello as Hello
import qualified Examples.Commands as Commands

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

checkHelpText :: Show a => String -> ParserInfo a -> Assertion
checkHelpText name p = do
  let result = execParserPure p ["--help"]
  assertLeft result $ \(ParserFailure err code) -> do
    expected <- readFile $ "tests/" ++ name ++ ".err.txt"
    expected @=? err name
    ExitFailure 1 @=? code

case_hello :: Assertion
case_hello = checkHelpText "hello" Hello.opts

case_modes :: Assertion
case_modes = checkHelpText "commands" Commands.opts

main :: IO ()
main = $(defaultMainGenerator)
