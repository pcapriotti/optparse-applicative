{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}
module Main where

import qualified Examples.Hello as Hello
import qualified Examples.Commands as Commands
import qualified Examples.Cabal as Cabal
import qualified Examples.Alternatives as Alternatives
import qualified Examples.Formatting as Formatting

import           Control.Applicative
import           Control.Monad
import           Data.List hiding (group)
import           Data.Semigroup hiding (option)

import           System.Exit
import           Test.QuickCheck hiding (Success, Failure)
import           Test.QuickCheck.Property

import           Options.Applicative
import           Options.Applicative.Types
import           Options.Applicative.Help.Pretty (Doc, SimpleDoc(..))
import qualified Options.Applicative.Help.Pretty as Doc
import           Options.Applicative.Help.Chunk

import           Prelude

run :: ParserInfo a -> [String] -> ParserResult a
run = execParserPure defaultPrefs

assertError :: Show a => ParserResult a
            -> (ParserFailure ParserHelp -> Property) -> Property
assertError x f = case x of
  Success r -> counterexample ("expected failure, got success: " ++ show r) failed
  Failure e -> f e
  CompletionInvoked _ -> counterexample "expected failure, got completion" failed

assertResult :: ParserResult a -> (a -> Property) -> Property
assertResult x f = case x of
  Success r -> f r
  Failure e -> do
    let (msg, _) = renderFailure e "test"
    counterexample ("unexpected parse error\n" ++ msg) failed
  CompletionInvoked _ -> counterexample "expected result, got completion" failed

assertHasLine :: String -> String -> Property
assertHasLine l s = counterexample ("expected line:\n\t" ++ l ++ "\nnot found")
                  $ l `elem` lines s

checkHelpTextWith :: Show a => ExitCode -> ParserPrefs -> String
                  -> ParserInfo a -> [String] -> Property
checkHelpTextWith ecode pprefs name p args = ioProperty $ do
  let result = execParserPure pprefs p args
  expected <- readFile $ "tests/" ++ name ++ ".err.txt"
  return $ assertError result $ \failure ->
    let (msg, code) = renderFailure failure name
    in  (expected === msg ++ "\n") .&&. (ecode === code)

checkHelpText :: Show a => String -> ParserInfo a -> [String] -> Property
checkHelpText = checkHelpTextWith ExitSuccess defaultPrefs

prop_hello :: Property
prop_hello = once $
  checkHelpText "hello" Hello.opts ["--help"]

prop_modes :: Property
prop_modes = once $
  checkHelpText "commands" Commands.opts ["--help"]

prop_cmd_header :: Property
prop_cmd_header = once $
  let i  = info (helper <*> Commands.sample) (header "foo")
      r1 = checkHelpTextWith (ExitFailure 1) defaultPrefs
                    "commands_header" i ["-zzz"]
      r2 = checkHelpTextWith (ExitFailure 1) (prefs showHelpOnError)
                    "commands_header_full" i ["-zzz"]
  in  (r1 .&&. r2)

prop_cabal_conf :: Property
prop_cabal_conf = once $
  checkHelpText "cabal" Cabal.pinfo ["configure", "--help"]

prop_args :: Property
prop_args = once $
  let result = run Commands.opts ["hello", "foo", "bar"]
  in  assertResult result ((===) (Commands.Hello ["foo", "bar"]))

prop_args_opts :: Property
prop_args_opts = once $
  let result = run Commands.opts ["hello", "foo", "--bar"]
  in  assertError result (\_ -> property succeeded)

prop_args_ddash :: Property
prop_args_ddash = once $
  let result = run Commands.opts ["hello", "foo", "--", "--bar", "--", "baz"]
  in  assertResult result ((===) (Commands.Hello ["foo", "--bar", "--", "baz"]))

prop_alts :: Property
prop_alts = once $
  let result = run Alternatives.opts ["-b", "-a", "-b", "-a", "-a", "-b"]
  in  assertResult result $ \xs ->
    let a = Alternatives.A
        b = Alternatives.B
    in  [b, a, b, a, a, b] === xs

prop_show_default :: Property
prop_show_default = once $
  let p = option auto
          ( short 'n'
          <> help "set count"
          <> value (0 :: Int)
          <> showDefault )
      i = info (p <**> helper) idm
      result = run i ["--help"]
  in  assertError result $ \failure ->
    let (msg, _) = renderFailure failure "test"
    in  assertHasLine
        "  -n ARG                   set count (default: 0)"
        msg

prop_alt_cont :: Property
prop_alt_cont = once $
  let p = Alternatives.a <|> Alternatives.b
      i = info p idm
      result = run i ["-a", "-b"]
  in  assertError result (\_ -> property succeeded)

prop_alt_help :: Property
prop_alt_help = once $
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
  in checkHelpText "alt" i ["--help"]

prop_nested_commands :: Property
prop_nested_commands = once $
  let p3 = strOption (short 'a' <> metavar "A")
      p2 = subparser (command "b" (info p3 idm))
      p1 = subparser (command "c" (info p2 idm))
      i = info (p1 <**> helper) idm
  in checkHelpTextWith (ExitFailure 1) defaultPrefs "nested" i ["c", "b"]

prop_drops_back_contexts :: Property
prop_drops_back_contexts = once $
  let p3 = strOption (short 'a' <> metavar "A")
      p2 = subparser (command "b" (info p3 idm)  <> metavar "B")
      p1 = subparser (command "c" (info p3 idm)  <> metavar "C")
      p0 = (,) <$> p2 <*> p1
      i = info (p0 <**> helper) idm
  in checkHelpTextWith (ExitFailure 1) defaultPrefs "dropback" i ["b", "-aA"]

prop_context_carry :: Property
prop_context_carry = once $
  let p3 = strOption (short 'a' <> metavar "A")
      p2 = subparser (command "b" (info p3 idm)  <> metavar "B")
      p1 = subparser (command "c" (info p3 idm)  <> metavar "C")
      p0 = (,) <$> p2 <*> p1
      i = info (p0 <**> helper) idm
  in checkHelpTextWith (ExitFailure 1) defaultPrefs "carry" i ["b", "-aA", "c"]

prop_help_on_empty :: Property
prop_help_on_empty = once $
  let p3 = strOption (short 'a' <> metavar "A")
      p2 = subparser (command "b" (info p3 idm)  <> metavar "B")
      p1 = subparser (command "c" (info p3 idm)  <> metavar "C")
      p0 = (,) <$> p2 <*> p1
      i = info (p0 <**> helper) idm
  in checkHelpTextWith (ExitFailure 1) (prefs showHelpOnEmpty) "helponempty" i []

prop_help_on_empty_sub :: Property
prop_help_on_empty_sub = once $
  let p3 = strOption (short 'a' <> metavar "A" <> help "both commands require this")
      p2 = subparser (command "b" (info p3 idm)  <> metavar "B")
      p1 = subparser (command "c" (info p3 idm)  <> metavar "C")
      p0 = (,) <$> p2 <*> p1
      i = info (p0 <**> helper) idm
  in checkHelpTextWith (ExitFailure 1) (prefs showHelpOnEmpty) "helponemptysub" i ["b", "-aA", "c"]

prop_many_args :: Property
prop_many_args = forAll (choose (0,2000)) $ \nargs ->
  let p = many (argument str idm)
      i = info p idm
      result = run i (replicate nargs "foo")
  in  assertResult result (\xs -> nargs === length xs)

prop_disambiguate :: Property
prop_disambiguate = once $
  let p =   flag' (1 :: Int) (long "foo")
        <|> flag' 2 (long "bar")
        <|> flag' 3 (long "baz")
      i = info p idm
      result = execParserPure (prefs disambiguate) i ["--f"]
  in  assertResult result ((===) 1)

prop_ambiguous :: Property
prop_ambiguous = once $
  let p =   flag' (1 :: Int) (long "foo")
        <|> flag' 2 (long "bar")
        <|> flag' 3 (long "baz")
      i = info p idm
      result = execParserPure (prefs disambiguate) i ["--ba"]
  in  assertError result (\_ -> property succeeded)

prop_completion :: Property
prop_completion = once . ioProperty $
  let p = (,)
        <$> strOption (long "foo" <> value "")
        <*> strOption (long "bar" <> value "")
      i = info p idm
      result = run i ["--bash-completion-index", "0"]
  in case result of
    CompletionInvoked (CompletionResult err) -> do
      completions <- lines <$> err "test"
      return $ ["--foo", "--bar"] === completions
    Failure _   -> return $ counterexample "unexpected failure" failed
    Success val -> return $ counterexample ("unexpected result " ++ show val) failed

prop_bind_usage :: Property
prop_bind_usage = once $
  let p = many (argument str (metavar "ARGS..."))
      i = info (p <**> helper) briefDesc
      result = run i ["--help"]
  in assertError result $ \failure ->
    let text = head . lines . fst $ renderFailure failure "test"
    in  "Usage: test [ARGS...]" === text

prop_issue_19 :: Property
prop_issue_19 = once $
  let p = option (fmap Just str)
        ( short 'x'
       <> value Nothing )
      i = info (p <**> helper) idm
      result = run i ["-x", "foo"]
  in  assertResult result (Just "foo" ===)

prop_arguments1_none :: Property
prop_arguments1_none =
  let p = some (argument str idm)
      i = info (p <**> helper) idm
      result = run i []
  in assertError result $ \_ -> property succeeded

prop_arguments1_some :: Property
prop_arguments1_some = once $
  let p = some (argument str idm)
      i = info (p <**> helper) idm
      result = run i ["foo", "--", "bar", "baz"]
  in  assertResult result (["foo", "bar", "baz"] ===)

prop_arguments_switch :: Property
prop_arguments_switch = once $
  let p =  switch (short 'x')
        *> many (argument str idm)
      i = info p idm
      result = run i ["--", "-x"]
  in assertResult result $ \args -> ["-x"] === args

prop_issue_35 :: Property
prop_issue_35 = once $
  let p =  flag' True (short 't' <> hidden)
       <|> flag' False (short 'f')
      i = info p idm
      result = run i []
  in assertError result $ \failure ->
    let text = lines . fst $ renderFailure failure "test"
    in  ["Missing: -f", "", "Usage: test -f"] === text

prop_backtracking :: Property
prop_backtracking = once $
  let p2 = switch (short 'a')
      p1 = (,)
        <$> subparser (command "c" (info p2 idm))
        <*> switch (short 'b')
      i = info (p1 <**> helper) idm
      result = execParserPure (prefs noBacktrack) i ["c", "-b"]
  in assertError result $ \_ -> property succeeded

prop_error_context :: Property
prop_error_context = once $
  let p = pk <$> option auto (long "port")
             <*> option auto (long "key")
      i = info p idm
      result = run i ["--port", "foo", "--key", "291"]
  in assertError result $ \failure ->
      let (msg, _) = renderFailure failure "test"
          errMsg   = head $ lines msg
      in  conjoin [ counterexample "no context in error message (option)" ("port" `isInfixOf` errMsg)
                  , counterexample "no context in error message (value)"  ("foo" `isInfixOf` errMsg)]
  where
    pk :: Int -> Int -> (Int, Int)
    pk = (,)

condr :: (Int -> Bool) -> ReadM Int
condr f = do
  x <- auto
  guard (f x)
  return x

prop_arg_order_1 :: Property
prop_arg_order_1 = once $
  let p = (,)
          <$> argument (condr even) idm
          <*> argument (condr odd) idm
      i = info p idm
      result = run i ["3", "6"]
  in assertError result $ \_ -> property succeeded

prop_arg_order_2 :: Property
prop_arg_order_2 = once $
  let p = (,,)
        <$> argument (condr even) idm
        <*> option (condr even) (short 'a')
        <*> option (condr odd) (short 'b')
      i = info p idm
      result = run i ["2", "-b", "3", "-a", "6"]
  in assertResult result ((===) (2, 6, 3))

prop_arg_order_3 :: Property
prop_arg_order_3 = once $
  let p = (,)
          <$> (  argument (condr even) idm
             <|> option auto (short 'n') )
          <*> argument (condr odd) idm
      i = info p idm
      result = run i ["-n", "3", "5"]
  in assertResult result ((===) (3, 5))

prop_unix_style :: Int -> Int -> Property
prop_unix_style j k =
  let p = (,)
          <$> flag' j (short 'x')
          <*> flag' k (short 'c')
      i = info p idm
      result = run i ["-xc"]
  in assertResult result ((===) (j,k))

prop_unix_with_options :: Property
prop_unix_with_options = once $
  let p = (,)
          <$> flag' (1 :: Int) (short 'x')
          <*> strOption (short 'a')
      i = info p idm
      result = run i ["-xac"]
  in assertResult result ((===) (1, "c"))

prop_count_flags :: Property
prop_count_flags = once $
  let p = length <$> many (flag' () (short 't'))
      i = info p idm
      result = run i ["-ttt"]
  in assertResult result ((===) 3)

prop_issue_47 :: Property
prop_issue_47 = once $
  let p = option r (long "test" <> value 9) :: Parser Int
      r = readerError "error message"
      result = run (info p idm) ["--test", "x"]
  in assertError result $ \failure ->
    let text = head . lines . fst $ renderFailure failure "test"
    in  counterexample "no error message" ("error message" `isInfixOf` text)

prop_long_help :: Property
prop_long_help = once $
  let p = Formatting.opts <**> helper
      i = info p
        ( progDesc (concat
            [ "This is a very long program description. "
            , "This text should be automatically wrapped "
            , "to fit the size of the terminal" ]) )
  in checkHelpTextWith ExitSuccess (prefs (columns 50)) "formatting" i ["--help"]

prop_issue_50 :: Property
prop_issue_50 = once $
  let p = argument str (metavar "INPUT")
          <* switch (long "version")
      result = run (info p idm) ["--version", "test"]
  in assertResult result $ \r -> "test" === r

prop_intersperse_1 :: Property
prop_intersperse_1 = once $
  let p = many (argument str (metavar "ARGS"))
          <* switch (short 'x')
      result = run (info p noIntersperse)
                 ["a", "-x", "b"]
  in assertResult result $ \args -> ["a", "-x", "b"] === args

prop_intersperse_2 :: Property
prop_intersperse_2 = once $
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
  in conjoin [ assertResult result1 $ \args -> ["-x", "foo"] === args
             , assertError result2 $ \_ -> property succeeded ]

prop_issue_52 :: Property
prop_issue_52 = once $
  let p = subparser
        ( metavar "FOO"
        <> command "run" (info (pure "foo") idm) )
      i = info p idm
  in assertError (run i []) $ \failure -> do
    let text = lines . fst $ renderFailure failure "test"
    ["Missing: FOO", "", "Usage: test FOO"] === text

prop_multiple_subparsers :: Property
prop_multiple_subparsers = once $
  let p1 = subparser
        (command "add" (info (pure ())
             ( progDesc "Add a file to the repository" )))
      p2 = subparser
        (command "commit" (info (pure ())
             ( progDesc "Record changes to the repository" )))
      i = info (p1 *> p2 <**> helper) idm
  in checkHelpText "subparsers" i ["--help"]

prop_argument_error :: Property
prop_argument_error = once $
  let r = (auto >>= \x -> x <$ guard (x == 42))
        <|> (str >>= \x -> readerError (x ++ " /= 42"))
      p1 = argument r idm :: Parser Int
      i = info (p1 *> p1) idm
  in assertError (run i ["3", "4"]) $ \failure ->
    let text = head . lines . fst $ renderFailure failure "test"
    in  "3 /= 42" === text

prop_reader_error_mplus :: Property
prop_reader_error_mplus = once $
  let r = (auto >>= \x -> x <$ guard (x == 42))
        <|> (str >>= \x -> readerError (x ++ " /= 42"))
      p1 = argument r idm :: Parser Int
      i = info p1 idm
  in assertError (run i ["foo"]) $ \failure ->
    let text = head . lines . fst $ renderFailure failure "test"
    in  "foo /= 42" === text

prop_missing_flags_described :: Property
prop_missing_flags_described = once $
  let p = (,,)
       <$> option str (short 'a')
       <*> option str (short 'b')
       <*> optional (option str (short 'c'))
      i = info p idm
  in assertError (run i ["-b", "3"]) $ \failure ->
    let text = head . lines . fst $ renderFailure failure "test"
    in  "Missing: -a ARG" === text

prop_many_missing_flags_described :: Property
prop_many_missing_flags_described = once $
  let p = (,)
        <$> option str (short 'a')
        <*> option str (short 'b')
      i = info p idm
  in assertError (run i []) $ \failure ->
    let text = head . lines . fst $ renderFailure failure "test"
    in  "Missing: -a ARG -b ARG" === text

prop_alt_missing_flags_described :: Property
prop_alt_missing_flags_described = once $
  let p = option str (short 'a') <|> option str (short 'b')
      i = info p idm
  in assertError (run i []) $ \failure ->
    let text = head . lines . fst $ renderFailure failure "test"
    in  "Missing: (-a ARG | -b ARG)" === text

prop_many_pairs_success :: Property
prop_many_pairs_success = once $
  let p = many $ (,) <$> argument str idm <*> argument str idm
      i = info p idm
      nargs = 10000
      result = run i (replicate nargs "foo")
  in assertResult result $ \xs -> nargs `div` 2 === length xs

prop_many_pairs_failure :: Property
prop_many_pairs_failure = once $
  let p = many $ (,) <$> argument str idm <*> argument str idm
      i = info p idm
      nargs = 9999
      result = run i (replicate nargs "foo")
  in assertError result $ \_ -> property succeeded

prop_many_pairs_lazy_progress :: Property
prop_many_pairs_lazy_progress = once $
  let p = many $ (,) <$> optional (option str (short 'a')) <*> argument str idm
      i = info p idm
      result = run i ["foo", "-abar", "baz"]
  in assertResult result $ \xs -> [(Just "bar", "foo"), (Nothing, "baz")] === xs

---

deriving instance Arbitrary a => Arbitrary (Chunk a)
deriving instance Eq SimpleDoc
deriving instance Show SimpleDoc

equalDocs :: Float -> Int -> Doc -> Doc -> Property
equalDocs f w d1 d2 = Doc.renderPretty f w d1
                  === Doc.renderPretty f w d2

prop_listToChunk_1 :: [String] -> Property
prop_listToChunk_1 xs = isEmpty (listToChunk xs) === null xs

prop_listToChunk_2 :: [String] -> Property
prop_listToChunk_2 xs = listToChunk xs === mconcat (fmap pure xs)

prop_extractChunk_1 :: String -> Property
prop_extractChunk_1 x = extractChunk (pure x) === x

prop_extractChunk_2 :: Chunk String -> Property
prop_extractChunk_2 x = extractChunk (fmap pure x) === x

prop_stringChunk_1 :: Positive Float -> Positive Int -> String -> Property
prop_stringChunk_1 (Positive f) (Positive w) s =
  equalDocs f w (extractChunk (stringChunk s))
                (Doc.string s)

prop_stringChunk_2 :: String -> Property
prop_stringChunk_2 s = isEmpty (stringChunk s) === null s

prop_paragraph :: String -> Property
prop_paragraph s = isEmpty (paragraph s) === null (words s)

---

return []
main :: IO ()
main = do
  result <- $(quickCheckAll)
  unless result exitFailure
