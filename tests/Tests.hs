{-# LANGUAGE TemplateHaskell #-}
module Main where

import qualified Examples.Hello as Hello
import qualified Examples.Commands as Commands
import qualified Examples.Cabal as Cabal
import qualified Examples.Alternatives as Alternatives

import Options.Applicative
import System.Exit
import Test.HUnit
import Test.Framework.Providers.HUnit
import Test.Framework.TH.Prime

run :: ParserInfo a -> [String] -> Either ParserFailure a
run = execParserPure (prefs idm)

assertLeft :: Show b => Either a b -> (a -> Assertion) -> Assertion
assertLeft x f = either f err x
  where
    err b = assertFailure $ "expected Left, got " ++ show b

assertHasLine :: String -> String -> Assertion
assertHasLine l s
  | l `elem` lines s = return ()
  | otherwise = assertFailure $ "expected line:\n\t" ++ l ++ "\nnot found"

checkHelpText :: Show a => String -> ParserInfo a -> [String] -> Assertion
checkHelpText name p args = do
  let result = run p args
  assertLeft result $ \(ParserFailure err code) -> do
    expected <- readFile $ "tests/" ++ name ++ ".err.txt"
    msg <- err name
    expected @=? msg
    ExitFailure 1 @=? code

case_hello :: Assertion
case_hello = checkHelpText "hello" Hello.opts ["--help"]

case_modes :: Assertion
case_modes = checkHelpText "commands" Commands.opts ["--help"]

case_cabal :: Assertion
case_cabal = checkHelpText "cabal" Cabal.pinfo ["configure", "--help"]

case_args :: Assertion
case_args = do
  let result = run Commands.opts ["hello", "foo", "bar"]
  case result of
    Left _ ->
      assertFailure "unexpected parse error"
    Right (Commands.Hello args) ->
      ["foo", "bar"] @=? args
    Right Commands.Goodbye ->
      assertFailure "unexpected result: Goodbye"

case_args_opts :: Assertion
case_args_opts = do
  let result = run Commands.opts ["hello", "foo", "--bar"]
  case result of
    Left _ -> return ()
    Right (Commands.Hello xs) ->
      assertFailure $ "unexpected result: Hello " ++ show xs
    Right Commands.Goodbye ->
      assertFailure "unexpected result: Goodbye"

case_args_ddash :: Assertion
case_args_ddash = do
  let result = run Commands.opts ["hello", "foo", "--", "--bar", "baz"]
  case result of
    Left _ ->
      assertFailure "unexpected parse error"
    Right (Commands.Hello args) ->
      ["foo", "--bar", "baz"] @=? args
    Right Commands.Goodbye ->
      assertFailure "unexpected result: Goodbye"

case_alts :: Assertion
case_alts = do
  let result = run Alternatives.opts ["-b", "-a", "-b", "-a", "-a", "-b"]
  case result of
    Left _ -> assertFailure "unexpected parse error"
    Right xs -> [b, a, b, a, a, b] @=? xs
      where a = Alternatives.A
            b = Alternatives.B

case_show_default :: Assertion
case_show_default = do
  let p = option ( short 'n'
                 & help "set count"
                 & value (0 :: Int)
                 & showDefault)
      i = info (p <**> helper) idm
      result = run i ["--help"]
  case result of
    Left (ParserFailure err _) -> do
      msg <- err "test"
      assertHasLine
        "  -n                       set count (default: 0)"
        msg
    Right r  -> assertFailure $ "unexpected result: " ++ show r

main :: IO ()
main = $(defaultMainGenerator)
