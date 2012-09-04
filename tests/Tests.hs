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
        "  -n ARG                   set count (default: 0)"
        msg
    Right r  -> assertFailure $ "unexpected result: " ++ show r

case_alt_cont :: Assertion
case_alt_cont = do
  let p = Alternatives.a <|> Alternatives.b
      i = info p idm
      result = run i ["-a", "-b"]
  case result of
    Left _ -> return ()
    Right r -> assertFailure $ "unexpected result: " ++ show r

case_alt_help :: Assertion
case_alt_help = do
  let p = p1 <|> p2 <|> p3
      p1 = (Just . Left)
        <$> strOption ( long "virtual-machine"
                      & metavar "VM"
                      & help "Virtual machine name" )
      p2 = (Just . Right)
        <$> strOption ( long "cloud-service"
                      & metavar "CS"
                      & help "Cloud service name" )
      p3 = flag' Nothing ( long "dry-run" )
      i = info (p <**> helper) idm
  checkHelpText "alt" i ["--help"]

case_nested_commands :: Assertion
case_nested_commands = do
  let p3 = strOption (short 'a' & metavar "A")
      p2 = subparser (command "b" (info p3 idm))
      p1 = subparser (command "c" (info p2 idm))
      i = info (p1 <**> helper) idm
  checkHelpText "nested" i ["c", "b"]

case_many_args :: Assertion
case_many_args = do
  let p = arguments str idm
      i = info p idm
      nargs = 20000
      result = run i (replicate nargs "foo")
  case result of
    Left _ -> assertFailure "unexpected parse error"
    Right xs -> nargs @=? length xs

case_disambiguate :: Assertion
case_disambiguate = do
  let p =   flag' (1 :: Int) (long "foo")
        <|> flag' 2 (long "bar")
        <|> flag' 3 (long "baz")
      i = info p idm
      result = execParserPure (prefs disambiguate) i ["--f"]
  case result of
    Left _ -> assertFailure "unexpected parse error"
    Right val -> 1 @=? val

case_ambiguous :: Assertion
case_ambiguous = do
  let p =   flag' (1 :: Int) (long "foo")
        <|> flag' 2 (long "bar")
        <|> flag' 3 (long "baz")
      i = info p idm
      result = execParserPure (prefs disambiguate) i ["--ba"]
  case result of
    Left _ -> return ()
    Right val -> assertFailure $ "unexpected result " ++ show val

case_completion :: Assertion
case_completion = do
  let p = (,)
        <$> strOption (long "foo" & value "")
        <*> strOption (long "bar" & value "")
      i = info p idm
      result = run i ["--bash-completion-index", "0"]
  case result of
    Left (ParserFailure err code) -> do
      ExitSuccess @=? code
      completions <- lines <$> err "test"
      ["--foo", "--bar"] @=? completions
    Right val ->
      assertFailure $ "unexpected result " ++ show val

case_bind_usage :: Assertion
case_bind_usage = do
  let p = arguments str (metavar "ARGS...")
      i = info (p <**> helper) briefDesc
      result = run i ["--help"]
  case result of
    Left (ParserFailure err _) -> do
      text <- head . lines <$> err "test"
      "Usage: test [ARGS...]" @=? text
    Right val ->
      assertFailure $ "unexpected result " ++ show val

case_issue_19 :: Assertion
case_issue_19 = do
  let p = option
        ( short 'x'
        & reader (Just . str)
        & value Nothing )
      i = info (p <**> helper) idm
      result = run i ["-x", "foo"]
  case result of
    Left _ -> assertFailure "unexpected parse error"
    Right r -> Just "foo" @=? r

main :: IO ()
main = $(defaultMainGenerator)
