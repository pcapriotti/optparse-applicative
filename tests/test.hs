{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE OverloadedStrings          #-}
module Main where

import qualified Examples.Hello as Hello
import qualified Examples.Commands as Commands
import qualified Examples.Cabal as Cabal
import qualified Examples.Alternatives as Alternatives
import qualified Examples.Formatting as Formatting
import qualified Examples.LongSub as LongSub
import qualified Examples.ParserGroup.AllGrouped as ParserGroup.AllGrouped
import qualified Examples.ParserGroup.Basic as ParserGroup.Basic
import qualified Examples.ParserGroup.CommandGroups as ParserGroup.CommandGroups
import qualified Examples.ParserGroup.DuplicateCommandGroups as ParserGroup.DuplicateCommandGroups
import qualified Examples.ParserGroup.Duplicates as ParserGroup.Duplicates
import qualified Examples.ParserGroup.Nested as ParserGroup.Nested

import System.OsString (OsString, osstr)
import qualified "os-string" System.OsString as OsString

import           Control.Applicative
import           Control.Monad
import           Data.List hiding (lines, group)
import           Data.List.NonEmpty (NonEmpty ((:|)))
import           Data.Semigroup hiding (option)
import           Data.String hiding (lines)

import           System.Exit
import           Test.QuickCheck hiding (Success, Failure)
import           Test.QuickCheck.Property

import           Options.Applicative
import           Options.Applicative.Types
import qualified Options.Applicative.NonEmpty


import qualified Options.Applicative.Help as H
import           Options.Applicative.Help.Pretty (Doc)
import qualified Options.Applicative.Help.Pretty as Doc
import           Options.Applicative.Help.Chunk
import           Options.Applicative.Help.Levenshtein

import           Prelude hiding (lines)
import qualified Data.Text.Lazy as Lazy
import qualified System.File.OsPath as OsPath
import qualified Data.Text.Lazy.Encoding as Lazy.Encoding
import qualified Data.Text as Strict
import Data.Coerce (coerce)
import Options.Applicative.Help.Pretty (osStringToStrictText)

run :: ParserInfo a -> [OsString] -> ParserResult a
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
    let (msg, _) = renderFailure e [osstr|test|]
    counterexample ("unexpected parse error\n" ++ Lazy.unpack msg) failed
  CompletionInvoked _ -> counterexample "expected result, got completion" failed

assertHasLine :: Lazy.Text -> Lazy.Text -> Property
assertHasLine l s = counterexample ("expected line:\n\t" ++ Lazy.unpack l ++ "\nnot found")
                  $ l `elem` Lazy.lines s

checkHelpTextWith :: Show a => ExitCode -> ParserPrefs -> OsString
                  -> ParserInfo a -> [OsString] -> Property
checkHelpTextWith ecode pprefs name p args = ioProperty $ do
  let result = execParserPure pprefs p args
  expected <- OsPath.readFile $ mconcat [[osstr|tests/|] <> name <>  [osstr|.err.txt|]]
  return $ assertError result $ \failure ->
    let (msg, code) = renderFailure failure name
    in  (expected === Lazy.Encoding.encodeUtf8 (msg <> "\n")) .&&. (ecode === code)

checkHelpText :: Show a => OsString -> ParserInfo a -> [OsString] -> Property
checkHelpText = checkHelpTextWith ExitSuccess defaultPrefs

prop_hello :: Property
prop_hello = once $
  checkHelpText [osstr|hello|] Hello.opts [[osstr|--help|]]

prop_modes :: Property
prop_modes = once $
  checkHelpText [osstr|commands|] Commands.opts [[osstr|--help|]]

prop_cmd_header :: Property
prop_cmd_header = once $
  let i  = info (helper <*> Commands.sample) (header "foo")
      r1 = checkHelpTextWith (ExitFailure 1) defaultPrefs
                    [osstr|commands_header|] i [[osstr|-zello|]]
      r2 = checkHelpTextWith (ExitFailure 1) (prefs showHelpOnError)
                    [osstr|commands_header_full|] i [[osstr|-zello|]]
  in  (r1 .&&. r2)

prop_cabal_conf :: Property
prop_cabal_conf = once $
  checkHelpTextWith ExitSuccess (prefs helpShowGlobals) [osstr|cabal|] Cabal.pinfo [[osstr|configure|], [osstr|--help|]]

prop_args :: Property
prop_args = once $
  let result = run Commands.opts [[osstr|hello|], [osstr|foo|], [osstr|bar|]]
  in  assertResult result (Commands.Hello [[osstr|foo|], [osstr|bar|]] ===)

prop_args_opts :: Property
prop_args_opts = once $
  let result = run Commands.opts [[osstr|hello|], [osstr|foo|], [osstr|--bar|]]
  in  assertError result (\_ -> property succeeded)

prop_args_ddash :: Property
prop_args_ddash = once $
  let result = run Commands.opts [[osstr|hello|], [osstr|foo|], [osstr|--|], [osstr|--bar|], [osstr|--|], [osstr|baz|]]
  in  assertResult result ((===) (Commands.Hello [[osstr|foo|], [osstr|--bar|], [osstr|--|], [osstr|baz|]]))

prop_alts :: Property
prop_alts = once $
  let result = run Alternatives.opts [[osstr|-b|], [osstr|-a|], [osstr|-b|], [osstr|-a|], [osstr|-a|], [osstr|-b|]]
  in  assertResult result $ \xs ->
    let a = Alternatives.A
        b = Alternatives.B
    in  [b, a, b, a, a, b] === xs

prop_show_default :: Property
prop_show_default = once $
  let p = option auto
          ( short (OsString.unsafeFromChar 'n')
          <> help "set count"
          <> value (0 :: Int)
          <> showDefault )
      i = info (p <**> helper) idm
      result = run i [[osstr|--help|]]
  in  assertError result $ \failure ->
    let (msg, _) = renderFailure failure [osstr|test|]
    in  assertHasLine
        "  -n ARG                   set count (default: 0)"
        msg

prop_alt_cont :: Property
prop_alt_cont = once $
  let p = Alternatives.a <|> Alternatives.b
      i = info p idm
      result = run i [[osstr|-a|], [osstr|-b|]]
  in  assertError result (\_ -> property succeeded)

prop_alt_help :: Property
prop_alt_help = once $
  let p :: Parser (Maybe (Either OsString OsString))
      p = p1 <|> p2 <|> p3
      p1 = (Just . Left)
        <$> osStrOption ( long [osstr|virtual-machine|]
                     <> metavar "VM"
                     <> help "Virtual machine name" )
      p2 = (Just . Right)
        <$> osStrOption ( long [osstr|cloud-service|]
                     <> metavar "CS"
                     <> help "Cloud service name" )
      p3 = flag' Nothing ( long [osstr|dry-run|] )
      i = info (p <**> helper) idm
  in checkHelpText [osstr|alt|] i [[osstr|--help|]]

prop_optional_help :: Property
prop_optional_help = once $
  let p :: Parser (Maybe (OsString, OsString))
      p = optional ((,)
                    <$> osStrOption ( long [osstr|a|]
                                    <> metavar "A"
                                    <> help "value a" )
                    <*> osStrOption ( long [osstr|b|]
                                    <> metavar "B"
                                    <> help "value b" ) )
      i = info (p <**> helper) idm
  in checkHelpText [osstr|optional|] i [[osstr|--help|]]

prop_optional_requiring_parens :: Property
prop_optional_requiring_parens = once $
  let p = optional $
            (,)
            <$> flag' () ( short (OsString.unsafeFromChar 'a') <> long [osstr|a|] )
            <*> flag' () ( short (OsString.unsafeFromChar 'b') <> long [osstr|b|] )
      i = info (p <**> helper) briefDesc
      result = run i [[osstr|--help|]]
  in assertError result $ \failure ->
    let text = head . Lazy.lines . fst $ renderFailure failure [osstr|test|]
    in  "Usage: test [(-a|--a) (-b|--b)]" === text

prop_optional_alt_requiring_parens :: Property
prop_optional_alt_requiring_parens = once $
  let p = optional $
                flag' () ( short (OsString.unsafeFromChar 'a') <> long [osstr|a|] )
            <|> flag' () ( short (OsString.unsafeFromChar 'b') <> long [osstr|b|] )
      i = info (p <**> helper) briefDesc
      result = run i [[osstr|--help|]]
  in assertError result $ \failure ->
    let text = head . Lazy.lines . fst $ renderFailure failure [osstr|test|]
    in  "Usage: test [(-a|--a) | (-b|--b)]" === text

prop_nested_optional_help :: Property
prop_nested_optional_help = once $
  let p :: Parser (OsString, Maybe (OsString, Maybe OsString))
      p = (,) <$>
          (osStrOption ( short (OsString.unsafeFromChar 'a')
                       <> long [osstr|a|]
                       <> metavar "A"
                       <> help "value a" ) ) <*>
          (optional
           ((,) <$>
            (osStrOption ( long [osstr|b0|]
                         <> metavar "B0"
                         <> help "value b0" ) ) <*>
            (optional (osStrOption ( long [osstr|b1|]
                                   <> metavar "B1"
                                   <> help "value b1" )))))
      i = info (p <**> helper) idm
  in checkHelpText [osstr|nested_optional|] i [[osstr|--help|]]

prop_long_equals :: Property
prop_long_equals = once $
  let p :: Parser OsString
      p = option osStr (   long [osstr|intval|]
                       <> short (OsString.unsafeFromChar 'j')
                       <> long [osstr|intval2|]
                       <> short (OsString.unsafeFromChar 'i')
                       <> help "integer value")
      i = info (p <**> helper) fullDesc
  in checkHelpTextWith ExitSuccess (prefs helpLongEquals) [osstr|long_equals|] i [[osstr|--help|]]

prop_long_equals_doesnt_do_shorts :: Property
prop_long_equals_doesnt_do_shorts = once $
  let p :: Parser OsString
      p = option osStr (   short (OsString.unsafeFromChar 'i')
                       <> help "integer value")
      i = info (p <**> helper) fullDesc
      result = execParserPure (prefs helpLongEquals) i [[osstr|--help|]]
  in assertError result $ \failure ->
    let text = head . Lazy.lines . fst $ renderFailure failure [osstr|test|]
    in  "Usage: test -i ARG" === text

prop_nested_fun :: Property
prop_nested_fun = once $
  let p :: Parser (OsString, Maybe (OsString, Maybe OsString))
      p = (,) <$>
          (osStrOption (short (OsString.unsafeFromChar 'a') <> long [osstr|a|] <> metavar "A")) <*>
          (optional
           ((,) <$>
            (osStrOption (short (OsString.unsafeFromChar 'b') <> long [osstr|b|] <> metavar "B")) <*>
            (optional (osStrOption (short (OsString.unsafeFromChar 'c') <> long [osstr|c|] <> metavar "C")))))
      i = info (p <**> helper) briefDesc
      result = run i [[osstr|--help|]]
  in assertError result $ \failure ->
    let text = head . Lazy.lines . fst $ renderFailure failure [osstr|test|]
    in  "Usage: test (-a|--a A) [(-b|--b B) [-c|--c C]]" === text

prop_nested_commands :: Property
prop_nested_commands = once $
  let p3 :: Parser OsString
      p3 = osStrOption (short (OsString.unsafeFromChar 'a') <> metavar "A")
      p2 = subparser (command [osstr|b|] (info p3 idm))
      p1 = subparser (command [osstr|c|] (info p2 idm))
      i = info (p1 <**> helper) idm
  in checkHelpTextWith (ExitFailure 1) defaultPrefs [osstr|nested|] i [[osstr|c|], [osstr|b|]]

prop_drops_back_contexts :: Property
prop_drops_back_contexts = once $
  let p3 :: Parser OsString
      p3 = osStrOption (short (OsString.unsafeFromChar 'a') <> metavar "A")
      p2 = subparser (command [osstr|b|] (info p3 idm)  <> metavar "B")
      p1 = subparser (command [osstr|c|] (info p3 idm)  <> metavar "C")
      p0 = (,) <$> p2 <*> p1
      i = info (p0 <**> helper) idm
  in checkHelpTextWith (ExitFailure 1) defaultPrefs [osstr|dropback|] i [[osstr|b|], [osstr|-aA|]]

prop_context_carry :: Property
prop_context_carry = once $
  let p3 :: Parser OsString
      p3 = osStrOption (short (OsString.unsafeFromChar 'a') <> metavar "A")
      p2 = subparser (command [osstr|b|] (info p3 idm)  <> metavar "B")
      p1 = subparser (command [osstr|c|] (info p3 idm)  <> metavar "C")
      p0 = (,) <$> p2 <*> p1
      i = info (p0 <**> helper) idm
  in checkHelpTextWith (ExitFailure 1) defaultPrefs [osstr|carry|] i [[osstr|b|], [osstr|-aA|], [osstr|c|]]

prop_help_on_empty :: Property
prop_help_on_empty = once $
  let p3 :: Parser OsString
      p3 = osStrOption (short (OsString.unsafeFromChar 'a') <> metavar "A")
      p2 = subparser (command [osstr|b|] (info p3 idm)  <> metavar "B")
      p1 = subparser (command [osstr|c|] (info p3 idm)  <> metavar "C")
      p0 = (,) <$> p2 <*> p1
      i = info (p0 <**> helper) idm
  in checkHelpTextWith (ExitFailure 1) (prefs showHelpOnEmpty) [osstr|helponempty|] i []

prop_help_on_empty_sub :: Property
prop_help_on_empty_sub = once $
  let p3 :: Parser OsString
      p3 = osStrOption (short (OsString.unsafeFromChar 'a') <> metavar "A" <> help "both commands require this")
      p2 = subparser (command [osstr|b|] (info p3 idm)  <> metavar "B")
      p1 = subparser (command [osstr|c|] (info p3 idm)  <> metavar "C")
      p0 = (,) <$> p2 <*> p1
      i = info (p0 <**> helper) idm
  in checkHelpTextWith (ExitFailure 1) (prefs showHelpOnEmpty) [osstr|helponemptysub|] i [[osstr|b|], [osstr|-aA|], [osstr|c|]]

prop_many_args :: Property
prop_many_args = forAll (choose (0,2000)) $ \nargs ->
  let p :: Parser [OsString]
      p = many (argument osStr idm)
      i = info p idm
      result = run i (replicate nargs [osstr|foo|])
  in  assertResult result (\xs -> nargs === length xs)

prop_disambiguate :: Property
prop_disambiguate = once $
  let p =   flag' (1 :: Int) (long [osstr|foo|])
        <|> flag' 2 (long [osstr|bar|])
        <|> flag' 3 (long [osstr|baz|])
      i = info p idm
      result = execParserPure (prefs disambiguate) i [[osstr|--f|]]
  in  assertResult result ((===) 1)

prop_ambiguous :: Property
prop_ambiguous = once $
  let p =   flag' (1 :: Int) (long [osstr|foo|])
        <|> flag' 2 (long [osstr|bar|])
        <|> flag' 3 (long [osstr|baz|])
      i = info p idm
      result = execParserPure (prefs disambiguate) i [[osstr|--ba|]]
  in  assertError result (\_ -> property succeeded)


prop_disambiguate_in_same_subparsers :: Property
prop_disambiguate_in_same_subparsers = once $
  let p0 = subparser (command [osstr|oranges|] (info (pure [osstr|oranges|]) idm) <> command [osstr|apples|] (info (pure [osstr|apples|]) idm) <> metavar "B")
      i = info (p0 <**> helper) idm
      result = execParserPure (prefs disambiguate) i [[osstr|orang|]]
  in  assertResult result ((===) [osstr|oranges|])

prop_disambiguate_commands_in_separate_subparsers :: Property
prop_disambiguate_commands_in_separate_subparsers = once $
  let p2 = subparser (command [osstr|oranges|] (info (pure [osstr|oranges|]) idm)  <> metavar "B")
      p1 = subparser (command [osstr|apples|] (info (pure [osstr|apples|]) idm)  <> metavar "C")
      p0 = p1 <|> p2
      i = info (p0 <**> helper) idm
      result = execParserPure (prefs disambiguate) i [[osstr|orang|]]
  in  assertResult result ((===) [osstr|oranges|])

prop_fail_ambiguous_commands_in_same_subparser :: Property
prop_fail_ambiguous_commands_in_same_subparser = once $
  let p0 = subparser (command [osstr|oranges|] (info (pure ()) idm) <> command [osstr|orangutans|] (info (pure ()) idm) <> metavar "B")
      i = info (p0 <**> helper) idm
      result = execParserPure (prefs disambiguate) i [[osstr|orang|]]
  in  assertError result (\_ -> property succeeded)

prop_fail_ambiguous_commands_in_separate_subparser :: Property
prop_fail_ambiguous_commands_in_separate_subparser = once $
  let p2 = subparser (command [osstr|oranges|] (info (pure ()) idm)  <> metavar "B")
      p1 = subparser (command [osstr|orangutans|] (info (pure ()) idm)  <> metavar "C")
      p0 = p1 <|> p2
      i = info (p0 <**> helper) idm
      result = execParserPure (prefs disambiguate) i [[osstr|orang|]]
  in  assertError result (\_ -> property succeeded)

prop_without_disambiguation_same_named_commands_should_parse_in_order :: Property
prop_without_disambiguation_same_named_commands_should_parse_in_order = once $
  let p3 = subparser (command [osstr|b|] (info (pure ()) idm)  <> metavar "B")
      p2 = subparser (command [osstr|a|] (info (pure ()) idm)  <> metavar "B")
      p1 = subparser (command [osstr|a|] (info (pure ()) idm)  <> metavar "C")
      p0 = (,,) <$> p1 <*> p2 <*> p3
      i = info (p0 <**> helper) idm
      result = execParserPure defaultPrefs i [[osstr|b|], [osstr|a|], [osstr|a|]]
  in  assertResult result ((===) ((), (), ()))

prop_completion :: Property
prop_completion = once . ioProperty $
  let p = (,)
        <$> osStrOption (long [osstr|foo|] <> value OsString.empty)
        <*> osStrOption (long [osstr|bar|] <> value OsString.empty)
      i = info p idm
      result = run i [[osstr|--bash-completion-index|], [osstr|0|]]
  in case result of
    CompletionInvoked (CompletionResult err) -> do
      completions <- Strict.lines <$> err [osstr|test|]
      return $ ["--foo", "--bar"] === completions
    Failure _   -> return $ counterexample "unexpected failure" failed
    Success val -> return $ counterexample ("unexpected result " ++ show val) failed

prop_completion_opt_after_double_dash :: Property
prop_completion_opt_after_double_dash = once . ioProperty $
  let p = (,)
        <$> osStrOption (long [osstr|foo|] <> value OsString.empty)
        <*> argument readerAsk (completeWith ["bar"])
      i = info p idm
      result = run i [[osstr|--bash-completion-index|], [osstr|2|]
                    , [osstr|--bash-completion-word|], [osstr|test|]
                    , [osstr|--bash-completion-word|], [osstr|--|]]
  in case result of
    CompletionInvoked (CompletionResult err) -> do
      completions <- Strict.lines <$> err [osstr|test|]
      return $ ["bar"] === completions
    Failure _   -> return $ counterexample "unexpected failure" failed
    Success val -> return $ counterexample ("unexpected result " ++ show val) failed

prop_completion_only_reachable :: Property
prop_completion_only_reachable = once . ioProperty $
  let p :: Parser (OsString, OsString)
      p = (,)
        <$> osStrArgument (completeWith ["reachable"])
        <*> osStrArgument (completeWith ["unreachable"])
      i = info p idm
      result = run i [[osstr|--bash-completion-index|], [osstr|0|]]
  in case result of
    CompletionInvoked (CompletionResult err) -> do
      completions <- Strict.lines <$> err [osstr|test|]
      return $ ["reachable"] === completions
    Failure _   -> return $ counterexample "unexpected failure" failed
    Success val -> return $ counterexample ("unexpected result " ++ show val) failed

prop_completion_only_reachable_deep :: Property
prop_completion_only_reachable_deep = once . ioProperty $
  let p :: Parser (OsString, OsString)
      p = (,)
        <$> osStrArgument (completeWith ["seen"])
        <*> osStrArgument (completeWith ["now-reachable"])
      i = info p idm
      result = run i [ [osstr|--bash-completion-index|], [osstr|2|]
                     , [osstr|--bash-completion-word|], [osstr|test-prog|]
                     , [osstr|--bash-completion-word|], [osstr|seen|] ]
  in case result of
    CompletionInvoked (CompletionResult err) -> do
      completions <- Strict.lines <$> err [osstr|test|]
      return $ ["now-reachable"] === completions
    Failure _   -> return $ counterexample "unexpected failure" failed
    Success val -> return $ counterexample ("unexpected result " ++ show val) failed

prop_completion_multi :: Property
prop_completion_multi = once . ioProperty $
  let p :: Parser [OsString]
      p = many (osStrArgument (completeWith ["reachable"]))
      i = info p idm
      result = run i [ [osstr|--bash-completion-index|], [osstr|3|]
                     , [osstr|--bash-completion-word|], [osstr|test-prog|]
                     , [osstr|--bash-completion-word|], [osstr|nope|] ]
  in case result of
    CompletionInvoked (CompletionResult err) -> do
      completions <- Strict.lines <$> err [osstr|test|]
      return $ ["reachable"] === completions
    Failure _   -> return $ counterexample "unexpected failure" failed
    Success val -> return $ counterexample ("unexpected result " ++ show val) failed

prop_completion_rich :: Property
prop_completion_rich = once . ioProperty $
  let p = (,)
        <$> option readerAsk (long [osstr|foo|] <> help "Fo?")
        <*> option readerAsk (long [osstr|bar|] <> help "Ba?")
      i = info p idm
      result = run i [[osstr|--bash-completion-enriched|], [osstr|--bash-completion-index|], [osstr|0|]]
  in case result of
    CompletionInvoked (CompletionResult err) -> do
      completions <- Strict.lines <$> err [osstr|test|]
      return $ ["--foo\tFo?", "--bar\tBa?"] === completions
    Failure _   -> return $ counterexample "unexpected failure" failed
    Success val -> return $ counterexample ("unexpected result " ++ show val) failed

prop_completion_rich_lengths :: Property
prop_completion_rich_lengths = once . ioProperty $
  let p = (,)
        <$> option readerAsk (long [osstr|foo|] <> help "Foo hide this")
        <*> option readerAsk (long [osstr|bar|] <> help "Bar hide this")
      i = info p idm
      result = run i [ [osstr|--bash-completion-enriched|]
                     , [osstr|--bash-completion-index=0|]
                     , [osstr|--bash-completion-option-desc-length=3|]
                     , [osstr|--bash-completion-command-desc-length=30|]]
  in case result of
    CompletionInvoked (CompletionResult err) -> do
      completions <- Strict.lines <$> err [osstr|test|]
      return $ ["--foo\tFoo...", "--bar\tBar..."] === completions
    Failure _   -> return $ counterexample "unexpected failure" failed
    Success val -> return $ counterexample ("unexpected result " ++ show val) failed

prop_bind_usage :: Property
prop_bind_usage = once $
  let p :: Parser [OsString]
      p = many (argument osStr (metavar "ARGS..."))
      i = info (p <**> helper) briefDesc
      result = run i [[osstr|--help|]]
  in assertError result $ \failure ->
    let text = head . Lazy.lines . fst $ renderFailure failure [osstr|test|]
    in  "Usage: test [ARGS...]" === text

prop_issue_19 :: Property
prop_issue_19 = once $
  let p = option (fmap Just osStr)
        ( short (OsString.unsafeFromChar 'x')
       <> value Nothing )
      i = info (p <**> helper) idm
      result = run i [[osstr|-x|], [osstr|foo|]]
  in  assertResult result (Just [osstr|foo|] ===)

prop_arguments1_none :: Property
prop_arguments1_none =
  let p :: Parser [OsString]
      p = some (argument osStr idm)
      i = info (p <**> helper) idm
      result = run i []
  in assertError result $ \_ -> property succeeded

prop_arguments1_some :: Property
prop_arguments1_some = once $
  let p :: Parser [OsString]
      p = some (argument osStr idm)
      i = info (p <**> helper) idm
      result = run i [[osstr|foo|], [osstr|--|], [osstr|bar|], [osstr|baz|]]
  in  assertResult result ([[osstr|foo|], [osstr|bar|], [osstr|baz|]] ===)

prop_arguments_switch :: Property
prop_arguments_switch = once $
  let p :: Parser [OsString]
      p =  switch (short (OsString.unsafeFromChar 'x'))
        *> many (argument osStr idm)
      i = info p idm
      result = run i [[osstr|--|], [osstr|-x|]]
  in assertResult result $ \args -> [[osstr|-x|]] === args

prop_issue_35 :: Property
prop_issue_35 = once $
  let p =  flag' True (short (OsString.unsafeFromChar 't') <> hidden)
       <|> flag' False (short (OsString.unsafeFromChar 'f'))
      i = info p idm
      result = run i []
  in assertError result $ \failure ->
    let text = Lazy.lines . fst $ renderFailure failure [osstr|test|]
    in  ["Missing: -f", Lazy.empty, "Usage: test -f"] === text

prop_backtracking :: Property
prop_backtracking = once $
  let p2 = switch (short (OsString.unsafeFromChar 'a'))
      p1 = (,)
        <$> subparser (command [osstr|c|] (info p2 idm))
        <*> switch (short (OsString.unsafeFromChar 'b'))
      i = info (p1 <**> helper) idm
      result = execParserPure (prefs noBacktrack) i [[osstr|c|], [osstr|-b|]]
  in assertError result $ \_ -> property succeeded

prop_subparser_inline :: Property
prop_subparser_inline = once $
  let p2 = switch (short (OsString.unsafeFromChar 'a'))
      p1 = (,)
        <$> subparser (command [osstr|c|] (info p2 idm))
        <*> switch (short (OsString.unsafeFromChar 'b'))
      i = info (p1 <**> helper) idm
      result = execParserPure (prefs subparserInline) i [[osstr|c|], [osstr|-b|], [osstr|-a|] ]
  in assertResult result ((True, True) ===)

prop_error_context :: Property
prop_error_context = once $
  let p = pk <$> option auto (long [osstr|port|])
             <*> option auto (long [osstr|key|])
      i = info p idm
      result = run i [[osstr|--port|], [osstr|foo|], [osstr|--key|], [osstr|291|]]
  in assertError result $ \failure ->
      let (msg, _) = renderFailure failure [osstr|test|]
          errMsg   = head $ Lazy.lines msg
      in  conjoin [ counterexample "no context in error message (option)" ("port" `Lazy.isInfixOf` errMsg)
                  , counterexample "no context in error message (value)"  ("foo" `Lazy.isInfixOf` errMsg)]
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
      result = run i [[osstr|3|], [osstr|6|]]
  in assertError result $ \_ -> property succeeded

prop_arg_order_2 :: Property
prop_arg_order_2 = once $
  let p = (,,)
        <$> argument (condr even) idm
        <*> option (condr even) (short (OsString.unsafeFromChar 'a'))
        <*> option (condr odd) (short (OsString.unsafeFromChar 'b'))
      i = info p idm
      result = run i [[osstr|2|], [osstr|-b|], [osstr|3|], [osstr|-a|], [osstr|6|]]
  in assertResult result ((===) (2, 6, 3))

prop_arg_order_3 :: Property
prop_arg_order_3 = once $
  let p = (,)
          <$> (  argument (condr even) idm
             <|> option auto (short (OsString.unsafeFromChar 'n')) )
          <*> argument (condr odd) idm
      i = info p idm
      result = run i [[osstr|-n|], [osstr|3|], [osstr|5|]]
  in assertResult result ((===) (3, 5))

prop_unix_style :: Int -> Int -> Property
prop_unix_style j k =
  let p = (,)
          <$> flag' j (short (OsString.unsafeFromChar 'x'))
          <*> flag' k (short (OsString.unsafeFromChar 'c'))
      i = info p idm
      result = run i [[osstr|-xc|]]
  in assertResult result ((===) (j,k))

prop_unix_with_options :: Property
prop_unix_with_options = once $
  let p = (,)
          <$> flag' (1 :: Int) (short (OsString.unsafeFromChar 'x'))
          <*> osStrOption (short (OsString.unsafeFromChar 'a'))
      i = info p idm
      result = run i [[osstr|-xac|]]
  in assertResult result ((1, [osstr|c|]) ===)

prop_count_flags :: Property
prop_count_flags = once $
  let p = length <$> many (flag' () (short (OsString.unsafeFromChar 't')))
      i = info p idm
      result = run i [[osstr|-ttt|]]
  in assertResult result ((===) 3)

prop_issue_47 :: Property
prop_issue_47 = once $
  let p = option r (long [osstr|test|] <> value 9) :: Parser Int
      r = readerError "error message"
      result = run (info p idm) [[osstr|--test|], [osstr|x|]]
  in assertError result $ \failure ->
    let text = head . Lazy.lines . fst $ renderFailure failure [osstr|test|]
    in  counterexample "no error message" ("error message" `Lazy.isInfixOf` text)

prop_long_help :: Property
prop_long_help = once $
  let p = Formatting.opts <**> helper
      i = info p
        ( progDesc (mconcat
            [ "This is a very long program description. "
            , "This text should be automatically wrapped "
            , "to fit the size of the terminal" ]) )
  in checkHelpTextWith ExitSuccess (prefs (columns 50)) [osstr|formatting|] i [[osstr|--help|]]

prop_issue_50 :: Property
prop_issue_50 = once $
  let p = argument osStr (metavar "INPUT")
          <* switch (long [osstr|version|])
      result = run (info p idm) [[osstr|--version|], [osstr|test|]]
  in assertResult result $ \r -> [osstr|test|] === r

prop_intersperse_1 :: Property
prop_intersperse_1 = once $
  let p = many (argument osStr (metavar "ARGS"))
          <* switch (short (OsString.unsafeFromChar 'x'))
      result = run (info p noIntersperse)
                 [[osstr|a|], [osstr|-x|], [osstr|b|]]
  in assertResult result $ \args -> [[osstr|a|], [osstr|-x|], [osstr|b|]] === args

prop_intersperse_2 :: Property
prop_intersperse_2 = once $
  let p = subparser
          (  command [osstr|run|]
             ( info (many (argument osStr (metavar "OPTIONS")))
                    noIntersperse )
          <> command [osstr|test|]
             ( info (many (argument osStr (metavar "ARGS")))
                    idm ) )
      i = info p idm
      result1 = run i [[osstr|run|], [osstr|foo|], [osstr|-x|]]
      result2 = run i [[osstr|test|], [osstr|bar|], [osstr|-x|]]
  in conjoin [ assertResult result1 $ \args -> [[osstr|foo|], [osstr|-x|]] === args
             , assertError result2 $ \_ -> property succeeded ]

prop_intersperse_3 :: Property
prop_intersperse_3 = once $
  let p = (,,) <$> switch ( long [osstr|foo|] )
               <*> osStrArgument ( metavar "FILE" )
               <*> many ( osStrArgument ( metavar "ARGS..." ) )
      i = info p noIntersperse
      result = run i [[osstr|--foo|], [osstr|myfile|], [osstr|-a|], [osstr|-b|], [osstr|-c|]]
  in assertResult result $ \(b,f,as) ->
     conjoin [ [[osstr|-a|], [osstr|-b|], [osstr|-c|]] === as
             , True               === b
             , [osstr|myfile|]           === f ]

prop_forward_options :: Property
prop_forward_options = once $
  let p = (,) <$> switch ( long [osstr|foo|] )
              <*> many ( osStrArgument ( metavar "ARGS..." ) )
      i = info p forwardOptions
      result = run i [[osstr|--fo|], [osstr|--foo|], [osstr|myfile|]]
  in assertResult result $ \(b,a) ->
     conjoin [ True               === b
             , [[osstr|--fo|], [osstr|myfile|]] === a ]

prop_issue_52 :: Property
prop_issue_52 = once $
  let p = subparser
        ( metavar "FOO"
        <> command [osstr|run|] (info (pure [osstr|foo|]) idm) )
      i = info p idm
  in assertError (run i []) $ \failure -> do
    let text = Lazy.lines . fst $ renderFailure failure [osstr|test|]
    ["Missing: FOO", Lazy.empty, "Usage: test FOO"] === text

prop_multiple_subparsers :: Property
prop_multiple_subparsers = once $
  let p1 = subparser
        (command [osstr|add|] (info (pure ())
             ( progDesc "Add a file to the repository" )))
      p2 = subparser
        (command [osstr|commit|] (info (pure ())
             ( progDesc "Record changes to the repository" )))
      i = info (p1 *> p2 <**> helper) idm
  in checkHelpText [osstr|subparsers|] i [[osstr|--help|]]

prop_argument_error :: Property
prop_argument_error = once $
  let r = (auto >>= \x -> x <$ guard (x == 42))
        <|> (osStr >>= \x -> readerError (osStringToStrictText (x <> [osstr| /= 42|])))
      p1 = argument r idm :: Parser Int
      i = info (p1 *> p1) idm
  in assertError (run i [[osstr|3|], [osstr|4|]]) $ \failure ->
    let text = head . Lazy.lines . fst $ renderFailure failure [osstr|test|]
    in  "3 /= 42" === text

prop_reader_error_mplus :: Property
prop_reader_error_mplus = once $
  let r = (auto >>= \x -> x <$ guard (x == 42))
        <|> (osStr >>= \x -> readerError (osStringToStrictText (x <> [osstr| /= 42|])))
      p1 = argument r idm :: Parser Int
      i = info p1 idm
  in assertError (run i [[osstr|foo|]]) $ \failure ->
    let text = head . Lazy.lines . fst $ renderFailure failure [osstr|test|]
    in  "foo /= 42" === text

prop_missing_flags_described :: Property
prop_missing_flags_described = once $
  let p :: Parser (OsString, OsString, Maybe OsString)
      p = (,,)
       <$> option osStr (short (OsString.unsafeFromChar 'a'))
       <*> option osStr (short (OsString.unsafeFromChar 'b'))
       <*> optional (option osStr (short (OsString.unsafeFromChar 'c')))
      i = info p idm
  in assertError (run i [[osstr|-b|], [osstr|3|]]) $ \failure ->
    let text = head . Lazy.lines . fst $ renderFailure failure [osstr|test|]
    in  "Missing: -a ARG" === text

prop_many_missing_flags_described :: Property
prop_many_missing_flags_described = once $
  let p :: Parser (OsString, OsString)
      p = (,)
        <$> option osStr (short (OsString.unsafeFromChar 'a'))
        <*> option osStr (short (OsString.unsafeFromChar 'b'))
      i = info p idm
  in assertError (run i []) $ \failure ->
    let text = head . Lazy.lines . fst $ renderFailure failure [osstr|test|]
    in  "Missing: -a ARG -b ARG" === text

prop_alt_missing_flags_described :: Property
prop_alt_missing_flags_described = once $
  let p :: Parser OsString
      p = option osStr (short (OsString.unsafeFromChar 'a')) <|> option osStr (short (OsString.unsafeFromChar 'b'))
      i = info p idm
  in assertError (run i []) $ \failure ->
    let text = head . Lazy.lines . fst $ renderFailure failure [osstr|test|]
    in  "Missing: (-a ARG | -b ARG)" === text

prop_missing_option_parameter_err :: Property
prop_missing_option_parameter_err = once $
  let p :: Parser OsString
      p = option osStr (short (OsString.unsafeFromChar 'a'))
      i = info p idm
  in assertError (run i [[osstr|-a|]]) $ \failure ->
    let text = head . Lazy.lines . fst $ renderFailure failure [osstr|test|]
    in  "The option `-a` expects an argument." === text

prop_many_pairs_success :: Property
prop_many_pairs_success = once $
  let p :: Parser [(OsString, OsString)]
      p = many $ (,) <$> argument osStr idm <*> argument osStr idm
      i = info p idm
      nargs = 10000
      result = run i (replicate nargs [osstr|foo|])
  in assertResult result $ \xs -> nargs `div` 2 === length xs

prop_many_pairs_failure :: Property
prop_many_pairs_failure = once $
  let p :: Parser [(OsString, OsString)]
      p = many $ (,) <$> argument osStr idm <*> argument osStr idm
      i = info p idm
      nargs = 9999
      result = run i (replicate nargs [osstr|foo|])
  in assertError result $ \_ -> property succeeded

prop_many_pairs_lazy_progress :: Property
prop_many_pairs_lazy_progress = once $
  let p :: Parser [(Maybe OsString, OsString)]
      p = many $ (,) <$> optional (option osStr (short (OsString.unsafeFromChar 'a'))) <*> argument osStr idm
      i = info p idm
      result = run i [[osstr|foo|], [osstr|-abar|], [osstr|baz|]]
  in assertResult result $ \xs -> [(Just [osstr|bar|], [osstr|foo|]), (Nothing, [osstr|baz|])] === xs

prop_suggest :: Property
prop_suggest = once $
  let p2 = subparser (command [osstr|first|]   (info (pure ()) idm))
      p1 = subparser (command [osstr|fst|]     (info (pure ()) idm))
      p3 = subparser (command [osstr|far-off|] (info (pure ()) idm))
      p  = p2 *> p1 *> p3
      i  = info p idm
      result = run i [[osstr|fist|]]
  in assertError result $ \failure ->
    let (msg, _)  = renderFailure failure [osstr|prog|]
    in  counterexample (Lazy.unpack msg)
       $  Lazy.isInfixOf "Did you mean one of these?\n    first\n    fst" msg

prop_grouped_some_option_ellipsis :: Property
prop_grouped_some_option_ellipsis = once $
  let x :: Parser OsString
      x = osStrOption (short (OsString.unsafeFromChar 'x') <> metavar "X")
      p = prefs (multiSuffix "...")
      r = show . extractChunk $ H.briefDesc p (x *> some x)
  in r === "-x X (-x X)..."

prop_grouped_many_option_ellipsis :: Property
prop_grouped_many_option_ellipsis = once $
  let x :: Parser OsString
      x = osStrOption (short (OsString.unsafeFromChar 'x') <> metavar "X")
      p = prefs (multiSuffix "...")
      r = show . extractChunk $ H.briefDesc p (x *> many x)
  in r === "-x X [-x X]..."

prop_grouped_some_argument_ellipsis :: Property
prop_grouped_some_argument_ellipsis = once $
  let x :: Parser OsString
      x = osStrArgument (metavar "X")
      p = prefs (multiSuffix "...")
      r = show . extractChunk $ H.briefDesc p (x *> some x)
  in r === "X X..."

prop_grouped_many_argument_ellipsis :: Property
prop_grouped_many_argument_ellipsis = once $
  let x :: Parser OsString
      x = osStrArgument (metavar "X")
      p = prefs (multiSuffix "...")
      r = show . extractChunk $ H.briefDesc p (x *> many x)
  in r === "X [X]..."

prop_grouped_some_pairs_argument_ellipsis :: Property
prop_grouped_some_pairs_argument_ellipsis = once $
  let x :: Parser OsString
      x = osStrArgument (metavar "X")
      p = prefs (multiSuffix "...")
      r = show . extractChunk $ H.briefDesc p (x *> some (x *> x))
  in r === "X (X X)..."

prop_grouped_many_pairs_argument_ellipsis :: Property
prop_grouped_many_pairs_argument_ellipsis = once $
  let x :: Parser OsString
      x = osStrArgument (metavar "X")
      p = prefs (multiSuffix "...")
      r = show . extractChunk $ H.briefDesc p (x *> many (x *> x))
  in r === "X [X X]..."

prop_grouped_some_dual_option_ellipsis :: Property
prop_grouped_some_dual_option_ellipsis = once $
  let x :: Parser OsString
      x = osStrOption (short (OsString.unsafeFromChar 'a') <> short (OsString.unsafeFromChar 'b') <> metavar "X")
      p = prefs (multiSuffix "...")
      r = show . extractChunk $ H.briefDesc p (x *> some x)
  in r === "(-a|-b X) (-a|-b X)..."

prop_grouped_many_dual_option_ellipsis :: Property
prop_grouped_many_dual_option_ellipsis = once $
  let x :: Parser OsString
      x = osStrOption (short (OsString.unsafeFromChar 'a') <> short (OsString.unsafeFromChar 'b') <> metavar "X")
      p = prefs (multiSuffix "...")
      r = show . extractChunk $ H.briefDesc p (x *> many x)
  in r === "(-a|-b X) [-a|-b X]..."

prop_grouped_some_dual_flag_ellipsis :: Property
prop_grouped_some_dual_flag_ellipsis = once $
  let x = flag' () (short (OsString.unsafeFromChar 'a') <> short (OsString.unsafeFromChar 'b'))
      p = prefs (multiSuffix "...")
      r = show . extractChunk $ H.briefDesc p (x *> some x)
  in r === "(-a|-b) (-a|-b)..."

prop_grouped_many_dual_flag_ellipsis :: Property
prop_grouped_many_dual_flag_ellipsis = once $
  let x = flag' () (short (OsString.unsafeFromChar 'a') <> short (OsString.unsafeFromChar 'b'))
      p = prefs (multiSuffix "...")
      r = show . extractChunk $ H.briefDesc p (x *> many x)
  in r === "(-a|-b) [-a|-b]..."

prop_issue_402 :: Property
prop_issue_402 = once $
  let x = some (flag' () (short (OsString.unsafeFromChar 'a'))) <|> some (flag' () (short (OsString.unsafeFromChar 'b') <> internal))
      p = prefs (multiSuffix "...")
      r = show . extractChunk $ H.briefDesc p x
  in r === "(-a)..."

prop_nice_some1 :: Property
prop_nice_some1 = once $
  let x = Options.Applicative.NonEmpty.some1 (flag' () (short (OsString.unsafeFromChar 'a')))
      p = prefs (multiSuffix "...")
      r = show . extractChunk $ H.briefDesc p x
  in r === "(-a)..."

prop_some1_works :: Property
prop_some1_works = once $
  let p = Options.Applicative.NonEmpty.some1 (flag' () (short (OsString.unsafeFromChar 'a')))
      i = info p idm
      result = run i [[osstr|-a|], [osstr|-a|]]
  in assertResult result $ \xs -> () :| [()] === xs

prop_help_contexts :: Property
prop_help_contexts = once $
  let
    grabHelpMessage (Failure failure) =
      let (msg, ExitSuccess) = renderFailure failure [osstr|<text>|]
      in msg
    grabHelpMessage _ = error "Parse did not render help text"

    i = Cabal.pinfo
    pre = run i [[osstr|install|], [osstr|--help|]]
    post = run i [[osstr|--help|], [osstr|install|]]
  in grabHelpMessage pre === grabHelpMessage post

prop_help_unknown_context :: Property
prop_help_unknown_context = once $
  let
    grabHelpMessage (Failure failure) =
      let (msg, ExitSuccess) = renderFailure failure [osstr|<text>|]
      in msg
    grabHelpMessage _ = error "Parse did not render help text"

    i = Cabal.pinfo
    pre = run i [[osstr|--help|]]
    post = run i [[osstr|--help|], [osstr|not-a-command|]]
  in grabHelpMessage pre === grabHelpMessage post


prop_long_command_line_flow :: Property
prop_long_command_line_flow = once $
  let p = LongSub.sample <**> helper
      i = info p
        ( progDesc (mconcat
            [ "This is a very long program description. "
            , "This text should be automatically wrapped "
            , "to fit the size of the terminal" ]) )
  in checkHelpTextWith ExitSuccess (prefs (columns 50)) [osstr|formatting-long-subcommand|] i [[osstr|hello-very-long-sub|], [osstr|--help|]]

prop_parser_group_basic :: Property
prop_parser_group_basic = once $
  checkHelpText [osstr|parser_group_basic|] ParserGroup.Basic.opts [[osstr|--help|]]

prop_parser_group_command_groups :: Property
prop_parser_group_command_groups = once $
  checkHelpText [osstr|parser_group_command_groups|] ParserGroup.CommandGroups.opts [[osstr|--help|]]

prop_parser_group_duplicate_command_groups :: Property
prop_parser_group_duplicate_command_groups = once $
  checkHelpText [osstr|parser_group_duplicate_command_groups|] ParserGroup.DuplicateCommandGroups.opts [[osstr|--help|]]

prop_parser_group_duplicates :: Property
prop_parser_group_duplicates = once $
  checkHelpText [osstr|parser_group_duplicates|] ParserGroup.Duplicates.opts [[osstr|--help|]]

prop_parser_group_all_grouped :: Property
prop_parser_group_all_grouped = once $
  checkHelpText [osstr|parser_group_all_grouped|] ParserGroup.AllGrouped.opts [[osstr|--help|]]

prop_parser_group_nested :: Property
prop_parser_group_nested = once $
  checkHelpText [osstr|parser_group_nested|] ParserGroup.Nested.opts [[osstr|--help|]]

prop_issue_450_subcommand_show_help_on_empty_inline :: Property
prop_issue_450_subcommand_show_help_on_empty_inline = once $
  let
    q = (,)
      <$> flag' () (short (OsString.unsafeFromChar 'a') <> help "supply a")
      <*> flag' () (short (OsString.unsafeFromChar 'b') <> help "supply b")

    p =
      subparser $
        command [osstr|foo|] $ info q $
        progDesc "Foo commands."

    i = info (p <**> helper) briefDesc
    result = execParserPure (prefs (showHelpOnEmpty <> subparserInline)) i [[osstr|foo|]]
  in assertError result $ \failure ->
    let text = Lazy.lines . fst $ renderFailure failure [osstr|test|]
    in ["Usage: test foo -a -b"
       , Lazy.empty
       ,"  Foo commands."
       , Lazy.empty
       ,"Available options:"
       ,"  -a                       supply a"
       ,"  -b                       supply b"] === text


prop_issue_450_ensure_missing_still_shows :: Property
prop_issue_450_ensure_missing_still_shows = once $
  let
    q = (,)
      <$> flag' () (short (OsString.unsafeFromChar 'a') <> help "supply a")
      <*> flag' () (short (OsString.unsafeFromChar 'b') <> help "supply b")

    p =
      subparser $
        command [osstr|foo|] $ info q $
        progDesc "Foo commands."

    i = info (p <**> helper) briefDesc
    result = execParserPure (prefs (showHelpOnEmpty <> subparserInline)) i [[osstr|foo|], [osstr|-a|]]
  in assertError result $ \failure ->
    let text = Lazy.lines . fst $ renderFailure failure [osstr|test|]
    in ["Missing: -b"
       , Lazy.empty
       ,"Usage: test foo -a -b"
       , Lazy.empty
       ,"  Foo commands."] === text


---

deriving instance Arbitrary a => Arbitrary (Chunk a)


equalDocs :: Double -> Int -> Doc -> Doc -> Property
equalDocs f w d1 d2 = Doc.prettyLazyText f w d1
                  === Doc.prettyLazyText f w d2

prop_listToChunk_1 :: [String] -> Property
prop_listToChunk_1 xs = isEmpty (listToChunk xs) === null xs

prop_listToChunk_2 :: [String] -> Property
prop_listToChunk_2 xs = listToChunk xs === mconcat (fmap pure xs)

prop_extractChunk_1 :: String -> Property
prop_extractChunk_1 x = extractChunk (pure x) === x

prop_extractChunk_2 :: Chunk String -> Property
prop_extractChunk_2 x = extractChunk (fmap pure x) === x

prop_stringChunk_1 :: Positive Double -> Positive Int -> Strict.Text -> Property
prop_stringChunk_1 (Positive f) (Positive w) s =
  equalDocs f w (extractChunk (stringChunk s))
                (Doc.pretty s)

prop_stringChunk_2 :: Strict.Text -> Property
prop_stringChunk_2 s = isEmpty (stringChunk s) === Strict.null s

prop_paragraph :: Strict.Text -> Property
prop_paragraph s = isEmpty (paragraph s) === null (Strict.words s)

---

--
-- From
-- https://en.wikipedia.org/wiki/Damerau%E2%80%93Levenshtein_distance
--
-- In information theory and computer science, the Damerau–Levenshtein
-- distance is a distance (string metric) between two strings, i.e.,
-- finite sequence of symbols, given by counting the minimum number
-- of operations needed to transform one string into the other, where
-- an operation is defined as an insertion, deletion, or substitution
-- of a single character, or a transposition of two adjacent characters.
--
prop_edit_distance_gezero :: String -> String -> Bool
prop_edit_distance_gezero a b = editDistance a b >= 0

prop_edit_insertion :: [Char] -> Char -> [Char] -> Property
prop_edit_insertion as i bs =
  editDistance (as ++ bs) (as ++ [i] ++ bs) === 1

prop_edit_symmetric :: [Char] -> [Char] -> Property
prop_edit_symmetric as bs =
  editDistance as bs === editDistance bs as

prop_edit_substitution :: [Char] -> [Char] -> Char -> Char -> Property
prop_edit_substitution as bs a b = a /= b ==>
  editDistance (as ++ [a] ++ bs) (as ++ [b] ++ bs) === 1

prop_edit_transposition :: [Char] -> [Char] -> Char -> Char -> Property
prop_edit_transposition as bs a b = a /= b ==>
  editDistance (as ++ [a,b] ++ bs) (as ++ [b,a] ++ bs) === 1

---
instance Arbitrary Strict.Text where
  arbitrary = Strict.pack <$> (coerce (arbitrary :: Gen PrintableString))

return []
main :: IO ()
main = do
  result <- $(quickCheckAll)
  unless result exitFailure
