{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving,
             TemplateHaskell, CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import qualified Examples.Hello as Hello
import qualified Examples.Commands as Commands
import qualified Examples.Cabal as Cabal
import qualified Examples.Alternatives as Alternatives
import qualified Examples.Formatting as Formatting

import Control.Monad
import Data.List hiding (group)
import Data.Monoid
  ( mconcat
#if __GLASGOW_HASKELL__ <= 702
  , Monoid
  , mappend
#endif
  )

import System.Exit
import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty.HUnit
import Test.Tasty.TH
import Test.QuickCheck (Positive (..))
import Test.QuickCheck.Arbitrary

import Options.Applicative
import Options.Applicative.Help.Pretty (Doc, SimpleDoc(..))
import qualified Options.Applicative.Help.Pretty as Doc
import Options.Applicative.Help.Chunk

#if __GLASGOW_HASKELL__ <= 702
(<>) :: Monoid a => a -> a -> a
(<>) = mappend
#endif

run :: ParserInfo a -> [String] -> ParserResult a
run = execParserPure (prefs idm)

assertError :: Show a => ParserResult a -> (ParserFailure -> Assertion) -> Assertion
assertError x f = case x of
  Success r -> assertFailure $ "expected failure, got success: " ++ show r
  Failure e -> f e
  CompletionInvoked _ -> assertFailure "expected failure, got completion"

assertResult :: ParserResult a -> (a -> Assertion) -> Assertion
assertResult x f = case x of
  Success r -> f r
  Failure e -> do
    let (msg, _) = execFailure e "test"
    assertFailure $ "unexpected parse error\n" ++ msg
  CompletionInvoked _ -> assertFailure "expected result, got completion"

assertHasLine :: String -> String -> Assertion
assertHasLine l s
  | l `elem` lines s = return ()
  | otherwise = assertFailure $ "expected line:\n\t" ++ l ++ "\nnot found"

checkHelpTextWith :: Show a => ExitCode -> ParserPrefs -> String
                  -> ParserInfo a -> [String] -> Assertion
checkHelpTextWith ecode pprefs name p args = do
  let result = execParserPure pprefs p args
  assertError result $ \(ParserFailure err) -> do
    expected <- readFile $ name ++ ".err.txt"
    let (msg, code) = err name
    expected @=? msg ++ "\n"
    ecode @=? code

checkHelpText :: Show a => String -> ParserInfo a -> [String] -> Assertion
checkHelpText = checkHelpTextWith ExitSuccess (prefs idm)

case_hello :: Assertion
case_hello = checkHelpText "hello" Hello.opts ["--help"]

case_modes :: Assertion
case_modes = checkHelpText "commands" Commands.opts ["--help"]

case_cmd_header :: Assertion
case_cmd_header = do
  let i = info (helper <*> Commands.sample) (header "foo")
  checkHelpTextWith (ExitFailure 1) (prefs idm)
                    "commands_header" i ["-zzz"]
  checkHelpTextWith (ExitFailure 1) (prefs showHelpOnError)
                    "commands_header_full" i ["-zzz"]

case_cabal_conf :: Assertion
case_cabal_conf = checkHelpText "cabal" Cabal.pinfo ["configure", "--help"]

case_args :: Assertion
case_args = do
  let result = run Commands.opts ["hello", "foo", "bar"]
  case result of
    Success (Commands.Hello args) ->
      ["foo", "bar"] @=? args
    Success Commands.Goodbye ->
      assertFailure "unexpected result: Goodbye"
    _ ->
      assertFailure "unexpected parse error"

case_args_opts :: Assertion
case_args_opts = do
  let result = run Commands.opts ["hello", "foo", "--bar"]
  case result of
    Success (Commands.Hello xs) ->
      assertFailure $ "unexpected result: Hello " ++ show xs
    Success Commands.Goodbye ->
      assertFailure "unexpected result: Goodbye"
    _ -> return ()

case_args_ddash :: Assertion
case_args_ddash = do
  let result = run Commands.opts ["hello", "foo", "--", "--bar", "baz"]
  case result of
    Success (Commands.Hello args) ->
      ["foo", "--bar", "baz"] @=? args
    Success Commands.Goodbye ->
      assertFailure "unexpected result: Goodbye"
    _ -> assertFailure "unexpected parse error"

case_alts :: Assertion
case_alts = do
  let result = run Alternatives.opts ["-b", "-a", "-b", "-a", "-a", "-b"]
  case result of
    Success xs -> [b, a, b, a, a, b] @=? xs
      where a = Alternatives.A
            b = Alternatives.B
    _ -> assertFailure "unexpected parse error"

case_show_default :: Assertion
case_show_default = do
  let p = option ( short 'n'
                <> help "set count"
                <> value (0 :: Int)
                <> showDefault)
      i = info (p <**> helper) idm
      result = run i ["--help"]
  case result of
    Failure (ParserFailure err) -> do
      let (msg, _) = err "test"
      assertHasLine
        "  -n ARG                   set count (default: 0)"
        msg
    Success r -> assertFailure $ "unexpected result: " ++ show r
    CompletionInvoked _ -> assertFailure "unexpected completion"

case_alt_cont :: Assertion
case_alt_cont = do
  let p = Alternatives.a <|> Alternatives.b
      i = info p idm
      result = run i ["-a", "-b"]
  case result of
    Success r -> assertFailure $ "unexpected result: " ++ show r
    _ -> return ()

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
  checkHelpTextWith (ExitFailure 1) (prefs idm) "nested" i ["c", "b"]

case_many_args :: Assertion
case_many_args = do
  let p = many (argument str idm)
      i = info p idm
      nargs = 20000
      result = run i (replicate nargs "foo")
  case result of
    Success xs -> nargs @=? length xs
    _ -> assertFailure "unexpected parse error"

case_disambiguate :: Assertion
case_disambiguate = do
  let p =   flag' (1 :: Int) (long "foo")
        <|> flag' 2 (long "bar")
        <|> flag' 3 (long "baz")
      i = info p idm
      result = execParserPure (prefs disambiguate) i ["--f"]
  case result of
    Success val -> 1 @=? val
    _ -> assertFailure "unexpected parse error"

case_ambiguous :: Assertion
case_ambiguous = do
  let p =   flag' (1 :: Int) (long "foo")
        <|> flag' 2 (long "bar")
        <|> flag' 3 (long "baz")
      i = info p idm
      result = execParserPure (prefs disambiguate) i ["--ba"]
  case result of
    Success val -> assertFailure $ "unexpected result " ++ show val
    _ -> return ()

case_completion :: Assertion
case_completion = do
  let p = (,)
        <$> strOption (long "foo"<> value "")
        <*> strOption (long "bar"<> value "")
      i = info p idm
      result = run i ["--bash-completion-index", "0"]
  case result of
    CompletionInvoked (CompletionResult err) -> do
      completions <- lines <$> err "test"
      ["--foo", "--bar"] @=? completions
    Failure _ -> assertFailure "unexpected failure"
    Success val -> assertFailure $ "unexpected result " ++ show val

case_bind_usage :: Assertion
case_bind_usage = do
  let p = many (argument str (metavar "ARGS..."))
      i = info (p <**> helper) briefDesc
      result = run i ["--help"]
  case result of
    Failure (ParserFailure err) -> do
      let text = head . lines . fst $ err "test"
      "Usage: test [ARGS...]" @=? text
    Success val ->
      assertFailure $ "unexpected result " ++ show val
    CompletionInvoked _ -> assertFailure "unexpected completion"

case_issue_19 :: Assertion
case_issue_19 = do
  let p = option
        ( short 'x'
       <> reader (fmap Just . str)
       <> value Nothing )
      i = info (p <**> helper) idm
      result = run i ["-x", "foo"]
  case result of
    Success r -> Just "foo" @=? r
    _ -> assertFailure "unexpected parse error"

case_arguments1_none :: Assertion
case_arguments1_none = do
  let p = some (argument str idm)
      i = info (p <**> helper) idm
      result = run i []
  assertError result $ \(ParserFailure _) -> return ()

case_arguments1_some :: Assertion
case_arguments1_some = do
  let p = some (argument str idm)
      i = info (p <**> helper) idm
      result = run i ["foo", "--", "bar", "baz"]
  case result of
    Success r -> ["foo", "bar", "baz"] @=? r
    _ -> assertFailure "unexpected parse error"

case_arguments_switch :: Assertion
case_arguments_switch = do
  let p =  switch (short 'x')
        *> many (argument str idm)
      i = info p idm
      result = run i ["--", "-x"]
  assertResult result $ \args -> ["-x"] @=? args

case_issue_35 :: Assertion
case_issue_35 = do
  let p =  flag' True (short 't' <> hidden)
       <|> flag' False (short 'f')
      i = info p idm
      result = run i []
  assertError result $ \(ParserFailure err) -> do
    let text = head . lines . fst . err $ "test"
    "Usage: test -f" @=? text

case_backtracking :: Assertion
case_backtracking = do
  let p2 = switch (short 'a')
      p1 = (,)
        <$> subparser (command "c" (info p2 idm))
        <*> switch (short 'b')
      i = info (p1 <**> helper) idm
      result = execParserPure (prefs noBacktrack) i ["c", "-b"]
  assertError result $ \ _ -> return ()

case_error_context :: Assertion
case_error_context = do
  let p = pk <$> option (long "port")
             <*> option (long "key")
      i = info p idm
      result = run i ["--port", "foo", "--key", "291"]
  assertError result $ \(ParserFailure err) -> do
      let (msg, _) = err "test"
      let errMsg = head $ lines msg
      assertBool "no context in error message (option)"
                 ("port" `isInfixOf` errMsg)
      assertBool "no context in error message (value)"
                 ("foo" `isInfixOf` errMsg)
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
  assertError result $ \_ -> return ()

case_arg_order_2 :: Assertion
case_arg_order_2 = do
  let p = (,,)
        <$> argument (condr even) idm
        <*> option (reader (condr even) <> short 'a')
        <*> option (reader (condr odd) <> short 'b')
      i = info p idm
      result = run i ["2", "-b", "3", "-a", "6"]
  case result of
    Success res -> (2, 6, 3) @=? res
    _ -> assertFailure "unexpected parse error"

case_arg_order_3 :: Assertion
case_arg_order_3 = do
  let p = (,)
          <$> (  argument (condr even) idm
             <|> option (short 'n') )
          <*> argument (condr odd) idm
      i = info p idm
      result = run i ["-n", "3", "5"]
  case result of
    Success res -> (3, 5) @=? res
    _ -> assertFailure "unexpected parse error"

case_issue_47 :: Assertion
case_issue_47 = do
  let p = nullOption (long "test" <> reader r <> value 9) :: Parser Int
      r _ = readerError "error message"
      result = run (info p idm) ["--test", "x"]
  assertError result $ \(ParserFailure err) -> do
    let text = head . lines . fst . err $ "test"
    assertBool "no error message"
               ("error message" `isInfixOf` text)

case_long_help :: Assertion
case_long_help = do
  let p = Formatting.opts <**> helper
      i = info p
        ( progDesc (concat
            [ "This is a very long program description. "
            , "This text should be automatically wrapped "
            , "to fit the size of the terminal" ]) )
  checkHelpTextWith ExitSuccess (prefs (columns 50)) "formatting" i ["--help"]

case_issue_50 :: Assertion
case_issue_50 = do
  let p = argument str (metavar "INPUT")
          <* switch (long "version")
      result = run (info p idm) ["--version", "test"]
  assertResult result $ \r -> "test" @=? r

case_intersperse_1 :: Assertion
case_intersperse_1 = do
  let p = many (argument str (metavar "ARGS"))
          <* switch (short 'x')
      result = run (info p noIntersperse)
                 ["a", "-x", "b"]
  assertResult result $ \args -> ["a", "-x", "b"] @=? args

case_intersperse_2 :: Assertion
case_intersperse_2 = do
  let p = subparser
          (  command "run"
             ( info (many (argument str (metavar "OPTIONS")))
                    noIntersperse )
          <> command "test"
             ( info (many (argument str (metavar "ARGS")))
                    idm ) )
      i = info p idm
      result1 = run i ["run", "-x", "foo"]
      result2 = run i ["test", "-x", "bar"]
  assertResult result1 $ \args -> ["-x", "foo"] @=? args
  assertError result2 $ \_ -> return ()

case_issue_52 :: Assertion
case_issue_52 = do
  let p = subparser
        ( metavar "FOO"
        <> command "run" (info (pure "foo") idm) )
      i = info p idm
  assertError (run i []) $ \(ParserFailure err) -> do
    let text = head . lines . fst . err $ "test"
    "Usage: test FOO" @=? text

case_multiple_subparsers :: Assertion
case_multiple_subparsers = do
  let p1 = subparser
        (command "add" (info (pure ())
             ( progDesc "Add a file to the repository" )))
      p2 = subparser
        (command "commit" (info (pure ())
             ( progDesc "Record changes to the repository" )))
      i = info (p1 *> p2 <**> helper) idm
  checkHelpText "subparsers" i ["--help"]

---

deriving instance Arbitrary a => Arbitrary (Chunk a)
deriving instance Eq SimpleDoc

equalDocs :: Float -> Int -> Doc -> Doc -> Bool
equalDocs f w d1 d2 = Doc.renderPretty f w d1
                   == Doc.renderPretty f w d2

prop_listToChunk_1 :: [String] -> Bool
prop_listToChunk_1 xs = isEmpty (listToChunk xs) == null xs

prop_listToChunk_2 :: [String] -> Bool
prop_listToChunk_2 xs = listToChunk xs == mconcat (fmap pure xs)

prop_extractChunk_1 :: String -> Bool
prop_extractChunk_1 x = extractChunk (pure x) == x

prop_extractChunk_2 :: Chunk String -> Bool
prop_extractChunk_2 x = extractChunk (fmap pure x) == x

prop_stringChunk_1 :: Positive Float -> Positive Int -> String -> Bool
prop_stringChunk_1 (Positive f) (Positive w) s =
  equalDocs f w (extractChunk (stringChunk s))
                (Doc.string s)

prop_stringChunk_2 :: String -> Bool
prop_stringChunk_2 s = isEmpty (stringChunk s) == null s

prop_paragraph :: String -> Bool
prop_paragraph s = isEmpty (paragraph s) == null (words s)

---

main :: IO ()
main = $(defaultMainGenerator)
