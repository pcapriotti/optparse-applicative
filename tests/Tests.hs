{-# LANGUAGE TemplateHaskell, CPP #-}
module Main where

import qualified Examples.Hello as Hello
import qualified Examples.Commands as Commands
import qualified Examples.Cabal as Cabal
import qualified Examples.Alternatives as Alternatives

import Control.Monad
import Data.List
import Options.Applicative
import System.Exit
import Test.HUnit
import Test.Framework.Providers.HUnit
import Test.Framework.TH.Prime

#if __GLASGOW_HASKELL__ <= 702
import Data.Monoid
(<>) :: Monoid a => a -> a -> a
(<>) = mappend
#endif

run :: ParserInfo a -> [String] -> Either ParserFailure a
run = execParserPure (prefs idm)

assertLeft :: Show b => Either a b -> (a -> Assertion) -> Assertion
assertLeft x f = either f err x
  where
    err b = assertFailure $ "expected Left, got " ++ show b

assertRight :: Either ParserFailure b -> (b -> Assertion) -> Assertion
assertRight x f = either err f x
  where
    err (ParserFailure e _) = do
      msg <- e "test"
      assertFailure $ "unexpected parse error\n" ++ msg

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

case_cabal_conf :: Assertion
case_cabal_conf = checkHelpText "cabal" Cabal.pinfo ["configure", "--help"]

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
                <> help "set count"
                <> value (0 :: Int)
                <> showDefault)
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
                     <> metavar "VM"
                     <> help "Virtual machine name" )
      p2 = (Just . Right)
        <$> strOption ( long "cloud-service"
                     <> metavar "CS"
                     <> help "Cloud service name" )
      p3 = flag' Nothing ( long "dry-run" )
      i = info (p <**> helper) idm
  checkHelpText "alt" i ["--help"]

case_nested_commands :: Assertion
case_nested_commands = do
  let p3 = strOption (short 'a' <> metavar "A")
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
        <$> strOption (long "foo"<> value "")
        <*> strOption (long "bar"<> value "")
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
       <> reader (fmap Just . str)
       <> value Nothing )
      i = info (p <**> helper) idm
      result = run i ["-x", "foo"]
  case result of
    Left _ -> assertFailure "unexpected parse error"
    Right r -> Just "foo" @=? r

case_arguments1_none :: Assertion
case_arguments1_none = do
  let p = arguments1 str idm
      i = info (p <**> helper) idm
      result = run i []
  assertLeft result $ \(ParserFailure _ _) -> return ()

case_arguments1_some :: Assertion
case_arguments1_some = do
  let p = arguments1 str idm
      i = info (p <**> helper) idm
      result = run i ["foo", "--", "bar", "baz"]
  case result of
    Left _ -> assertFailure "unexpected parse error"
    Right r -> ["foo", "bar", "baz"] @=? r

case_arguments_switch :: Assertion
case_arguments_switch = do
  let p =  switch (short 'x')
        *> arguments str idm
      i = info p idm
      result = run i ["--", "-x"]
  assertRight result $ \args -> ["-x"] @=? args

case_issue_35 :: Assertion
case_issue_35 = do
  let p =  flag' True (short 't' <> hidden)
       <|> flag' False (short 'f')
      i = info p idm
      result = run i []
  case result of
    Left (ParserFailure err _) -> do
      text <- head . lines <$> err "test"
      "Usage: test -f" @=? text
    Right val ->
      assertFailure $ "unexpected result " ++ show val

case_backtracking :: Assertion
case_backtracking = do
  let p2 = switch (short 'a')
      p1 = (,)
        <$> subparser (command "c" (info p2 idm))
        <*> switch (short 'b')
      i = info (p1 <**> helper) idm
      result = execParserPure (prefs noBacktrack) i ["c", "-b"]
  assertLeft result $ \ _ -> return ()

case_error_context :: Assertion
case_error_context = do
  let p = pk <$> option (long "port")
             <*> option (long "key")
      i = info p idm
      result = run i ["--port", "foo", "--key", "291"]
  case result of
    Left (ParserFailure err _) -> do
      msg <- err "test"
      let errMsg = head $ lines msg
      assertBool "no context in error message (option)"
                 ("port" `isInfixOf` errMsg)
      assertBool "no context in error message (value)"
                 ("foo" `isInfixOf` errMsg)
    Right val ->
      assertFailure $ "unexpected result " ++ show val
  where
    pk :: Int -> Int -> (Int, Int)
    pk = (,)

condr :: MonadPlus m => (Int -> Bool) -> String -> m Int
condr f arg = do
  x <- auto arg
  guard (f (x :: Int))
  return x

case_arg_order_1 :: Assertion
case_arg_order_1 = do
  let p = (,)
          <$> argument (condr even) idm
          <*> argument (condr odd) idm
      i = info p idm
      result = run i ["3", "6"]
  assertLeft result $ \_ -> return ()

case_arg_order_2 :: Assertion
case_arg_order_2 = do
  let p = (,,)
        <$> argument (condr even) idm
        <*> option (reader (condr even) <> short 'a')
        <*> option (reader (condr odd) <> short 'b')
      i = info p idm
      result = run i ["2", "-b", "3", "-a", "6"]
  case result of
    Left _ -> assertFailure "unexpected parse error"
    Right res -> (2, 6, 3) @=? res

case_arg_order_3 :: Assertion
case_arg_order_3 = do
  let p = (,)
          <$> (  argument (condr even) idm
             <|> option (short 'n') )
          <*> argument (condr odd) idm
      i = info p idm
      result = run i ["-n", "3", "5"]
  case result of
    Left _ ->
      assertFailure "unexpected parse error"
    Right res -> (3, 5) @=? res

case_issue_47 :: Assertion
case_issue_47 = do
  let p = nullOption (long "test" <> reader r <> value 9) :: Parser Int
      r _ = readerError "error message"
      result = run (info p idm) ["--test", "x"]
  assertLeft result $ \(ParserFailure err _) -> do
    text <- head . lines <$> err "test"
    assertBool "no error message"
               ("error message" `isInfixOf` text)

case_issue_50 :: Assertion
case_issue_50 = do
  let p = argument str (metavar "INPUT")
          <* switch (long "version")
      result = run (info p idm) ["--version", "test"]
  assertRight result $ \r -> "test" @=? r

case_intersperse_1 :: Assertion
case_intersperse_1 = do
  let p = arguments str (metavar "ARGS")
          <* switch (short 'x')
      result = run (info p noIntersperse)
                 ["a", "-x", "b"]
  assertRight result $ \args -> ["a", "-x", "b"] @=? args

main :: IO ()
main = $(defaultMainGenerator)
