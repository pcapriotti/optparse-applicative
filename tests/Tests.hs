{-# LANGUAGE TemplateHaskell #-}
module Main where

import qualified Examples.Hello as Hello
import qualified Examples.Commands as Commands
import qualified Examples.Cabal as Cabal
import qualified Examples.Alternatives as Alternatives

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

case_args :: Assertion
case_args = do
  let result = execParserPure Commands.opts ["hello", "foo", "bar"]
  case result of
    Left _ ->
      assertFailure "unexpected parse error"
    Right (Commands.Hello args) ->
      ["foo", "bar"] @=? args
    Right Commands.Goodbye ->
      assertFailure "unexpected result: Goodbye"

case_args_opts :: Assertion
case_args_opts = do
  let result = execParserPure Commands.opts ["hello", "foo", "--bar"]
  case result of
    Left _ -> return ()
    Right (Commands.Hello xs) ->
      assertFailure $ "unexpected result: Hello " ++ show xs
    Right Commands.Goodbye ->
      assertFailure "unexpected result: Goodbye"

case_args_ddash :: Assertion
case_args_ddash = do
  let result = execParserPure Commands.opts ["hello", "foo", "--", "--bar", "baz"]
  case result of
    Left _ ->
      assertFailure "unexpected parse error"
    Right (Commands.Hello args) ->
      ["foo", "--bar", "baz"] @=? args
    Right Commands.Goodbye ->
      assertFailure "unexpected result: Goodbye"

case_alts :: Assertion
case_alts = do
  let result = execParserPure Alternatives.opts ["-b", "-a", "-b", "-a", "-a", "-b"]
  case result of
    Left _ -> assertFailure "unexpected parse error"
    Right xs -> [b, a, b, a, a, b] @=? xs
      where a = Alternatives.A
            b = Alternatives.B

main :: IO ()
main = $(defaultMainGenerator)
