-- Pure public-API issue461 controls; no shell completion, files or network.
module Examples.GlobalAlternatives (observations) where

import Control.Applicative
import Data.List (isInfixOf)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Semigroup ((<>))
import Options.Applicative
import Options.Applicative.Builder.Internal (noGlobal)
import System.Exit

data Scenario = Basic | Swapped | Nested | Aliased | SameSpelling | Suppressed
  deriving (Eq, Show)

parser :: Scenario -> ParserInfo (Bool, String)
parser scenario = info (((,) <$> verbose <*> alternatives) <**> helper) mempty
  where
    verbose = switch (long "verbose" <> help "SHARED_VERBOSITY" <>
      if scenario == Suppressed then noGlobal else mempty)
    runOptions = if scenario == SameSpelling
      then const "run" <$> strOption
        (long "port" <> value "child" <> help "RUN_PORT")
      else pure "run"
    runInfo = info (runOptions <**> helper) mempty
    commands = hsubparser (if scenario == Aliased
      then commandWithAliases ("run" :| ["r"]) runInfo
      else command "run" runInfo)
    nested = hsubparser (command "admin" (info
      ((switch (long "trace" <> help "SHARED_TRACE") *> commands)
        <**> helper) mempty))
    selected = if scenario == Nested then nested else commands
    launch = const "launch" <$> strOption
      (long "port" <> value "5050" <> help "LAUNCH_PORT")
    alternatives = if scenario == Swapped
      then launch <|> selected
      else selected <|> launch

globalText :: String -> String
globalText = unlines . drop 1 . dropWhile (/= "Global options:") . lines

helpChecks :: Scenario -> [String] -> [(String, Bool)]
helpChecks = helpChecksWith (prefs helpShowGlobals)

helpChecksWith :: ParserPrefs -> Scenario -> [String] -> [(String, Bool)]
helpChecksWith settings scenario args = case execParserPure settings (parser scenario) args of
  Failure failure ->
    let (message, code) = renderFailure failure "issue461"
        globals = globalText message
    in [("help exits successfully", code == ExitSuccess)
       ,("launch sibling is excluded", not ("LAUNCH_PORT" `isInfixOf` globals))
       ,("shared verbosity survives", "SHARED_VERBOSITY" `isInfixOf` globals)
       ] ++ [("nested ancestor survives", "SHARED_TRACE" `isInfixOf` globals)
               | scenario == Nested]
         ++ [("selected same-spelling child survives", "RUN_PORT" `isInfixOf` message)
               | scenario == SameSpelling]
  _ -> [("expected actual help Failure", False)]

parseCheck :: Scenario -> [String] -> (Bool, String) -> Bool
parseCheck scenario args expected = case execParserPure defaultPrefs
    (parser scenario) args of
  Success actual -> actual == expected
  _ -> False

helpControl :: ParserPrefs -> [String] -> (String -> Bool) -> Bool
helpControl settings args check = case execParserPure settings (parser Basic) args of
  Failure failure -> let (message, code) = renderFailure failure "issue461"
                     in code == ExitSuccess && check message
  _ -> False

helpScenarios :: [(Scenario, [String])]
helpScenarios =
  [(Basic, ["run", "--help"])
  ,(Basic, ["--help", "run"])
  ,(Basic, ["--verbose", "run", "--help"])
  ,(Swapped, ["run", "--help"])
  ,(Swapped, ["--help", "run"])
  ,(Nested, ["admin", "run", "--help"])
  ,(Nested, ["admin", "--help", "run"])
  ,(Aliased, ["r", "--help"])
  ,(SameSpelling, ["run", "--help"])]

observations :: [(String, Bool)]
observations =
  [(show scenario ++ " " ++ show args ++ ": " ++ label, passed)
   | (scenario, args) <- helpScenarios
   , (label, passed) <- helpChecks scenario args]
  ++ [("default launch parsing", parseCheck Basic [] (False, "launch"))
     ,("explicit launch option parsing", parseCheck Basic ["--port", "5051"] (False, "launch"))
     ,("run parsing", parseCheck Basic ["run"] (False, "run"))
     ,("supplied global parsing", parseCheck Basic ["--verbose", "run"] (True, "run"))
     ,("swapped parsing", parseCheck Swapped ["run"] (False, "run"))
     ,("nested parsing", parseCheck Nested ["admin", "run"] (False, "run"))
     ,("alias parsing", parseCheck Aliased ["r"] (False, "run"))
     ,("same spelling selected parsing", parseCheck SameSpelling ["run", "--port", "9090"] (False, "run"))
     ,("launch-only option remains invalid after run", case execParserPure defaultPrefs
         (parser Basic) ["run", "--port", "5050"] of
           Failure failure -> let (message, code) = renderFailure failure "issue461"
                              in code /= ExitSuccess &&
                                 "Invalid option" `isInfixOf` message &&
                                 "--port" `isInfixOf` message
           _ -> False)
     ,("root help retains launch option", helpControl (prefs helpShowGlobals)
         ["--help"] (isInfixOf "LAUNCH_PORT"))
     ,("unknown help context remains root", helpControl (prefs helpShowGlobals)
         ["--help", "not-a-command"] (isInfixOf "LAUNCH_PORT"))
     ,("disabled globals remain absent", helpControl defaultPrefs
         ["run", "--help"] (null . globalText))
     ,("noGlobal shared option remains suppressed", case execParserPure
         (prefs helpShowGlobals) (parser Suppressed) ["run", "--help"] of
           Failure failure -> let (message, code) = renderFailure failure "issue461"
                              in code == ExitSuccess && not
                                ("SHARED_VERBOSITY" `isInfixOf` globalText message)
           _ -> False)
     ]
  ++ sameNameObservations
  ++ [("Inline " ++ show scenario ++ " " ++ show args ++ ": " ++ label, passed)
     | (scenario, args) <- helpScenarios
     , (label, passed) <- helpChecksWith (prefs (helpShowGlobals <> subparserInline)) scenario args]
  ++ repeatedGlobalObservations
  ++ helpAlternativeObservations

-- Both structural alternatives contain a command named "run". Its command
-- name alone cannot identify which branch's original parent help to retain.
-- Required flags select the branch before entering genuinely different child
-- parsers. Shared verbosity is supplied earlier and must not disappear.
sameNameParser :: Bool -> ParserInfo (Bool, String)
sameNameParser swapped = info (((,) <$> verbose <*> alternatives) <**> helper) mempty
  where
    verbose = switch (long "verbose" <> help "SHARED_VERBOSITY")
    left = flag' () (long "choose-left" <> help "LEFT_PREREQUISITE") *>
      hsubparser (command "run" (info
        ((("left:" ++) <$> strOption (long "left-value" <> value "left-default" <>
          help "LEFT_RUN_CONTENT")) <**> helper) mempty))
    right = flag' () (long "choose-right" <> help "RIGHT_PREREQUISITE") *>
      hsubparser (command "run" (info
        ((("right:" ++) <$> strOption (long "right-value" <> value "right-default" <>
          help "RIGHT_RUN_CONTENT")) <**> helper) mempty))
    alternatives = if swapped then right <|> left else left <|> right

sameNameObservations :: [(String, Bool)]
sameNameObservations = concat
  [ checks swapped side
  | swapped <- [False, True]
  , side <- ["left", "right"]
  ]
  where
    checks swapped side = helpRows ++ [parseRow, invalidRow]
      where
        title = "SameName " ++ (if swapped then "swapped " else "normal ") ++ side ++ ": "
        prefix = ["--verbose", "--choose-" ++ side, "run"]
        other = if side == "left" then "right" else "left"
        selectedPrerequisite = if side == "left" then "LEFT_PREREQUISITE" else "RIGHT_PREREQUISITE"
        siblingPrerequisite = if side == "left" then "RIGHT_PREREQUISITE" else "LEFT_PREREQUISITE"
        selectedContent = if side == "left" then "LEFT_RUN_CONTENT" else "RIGHT_RUN_CONTENT"
        siblingContent = if side == "left" then "RIGHT_RUN_CONTENT" else "LEFT_RUN_CONTENT"
        helpRows = concatMap checkHelp
          [prefix ++ ["--help"], ["--verbose", "--choose-" ++ side, "--help", "run"]]
        checkHelp args = case execParserPure (prefs helpShowGlobals)
            (sameNameParser swapped) args of
          Failure failure ->
            let (message, code) = renderFailure failure "issue461"
                globals = globalText message
            in map (\(name, passed) -> (title ++ show args ++ " " ++ name, passed))
              [("help exits successfully", code == ExitSuccess)
              ,("selected prerequisite survives", selectedPrerequisite `isInfixOf` globals)
              ,("sibling prerequisite excluded", not (siblingPrerequisite `isInfixOf` globals))
              ,("selected child help survives", selectedContent `isInfixOf` message)
              ,("sibling child help excluded", not (siblingContent `isInfixOf` message))
              ,("supplied shared global survives", "SHARED_VERBOSITY" `isInfixOf` globals)
              ]
          _ -> [(title ++ "expected actual help Failure", False)]
        parseRow = (title ++ "selected branch parses", case execParserPure defaultPrefs
            (sameNameParser swapped) (prefix ++ ["--" ++ side ++ "-value", "chosen"]) of
          Success actual -> actual == (True, side ++ ":chosen")
          _ -> False)
        invalidRow = (title ++ "wrong-branch child rejected specifically", case execParserPure defaultPrefs
            (sameNameParser swapped) (prefix ++ ["--" ++ other ++ "-value", "chosen"]) of
          Failure failure -> let (message, code) = renderFailure failure "issue461"
                             in code /= ExitSuccess &&
                                "Invalid option" `isInfixOf` message &&
                                ("--" ++ other ++ "-value") `isInfixOf` message
          _ -> False)

-- Repeated options use BindP internally. Keep their original help once,
-- even after consuming several values before an applicative command choice.
repeatedGlobalObservations :: [(String, Bool)]
repeatedGlobalObservations =
  case execParserPure (prefs helpShowGlobals) spec
      ["--include", "one", "--include", "two", "run", "--help"] of
    Failure failure ->
      let (message, code) = renderFailure failure "issue461"
          globals = globalText message
      in [("repeated global help succeeds", code == ExitSuccess)
         ,("consumed repeated global survives once", length
            (filter (isInfixOf "INCLUDE_PATH") (lines globals)) == 1)
         ,("repeated global does not expose sibling", not ("REPEAT_LAUNCH" `isInfixOf` globals))]
    _ -> [("expected repeated global help", False)]
  where
    spec = info (((,) <$> includes <*> branches) <**> helper) mempty
    includes :: Parser [String]
    includes = many (strOption (long "include" <> help "INCLUDE_PATH"))
    branches = hsubparser (command "run" (info (pure () <**> helper) mempty))
      <|> (() <$ strOption (long "port" <> value "5050" <> help "REPEAT_LAUNCH"))

-- An aborting help reader has not accepted its parser alternative. Keep the
-- command alternative available for the virtual --help COMMAND lookup.
helpAlternativeObservations :: [(String, Bool)]
helpAlternativeObservations = concatMap check [["--help", "run"], ["run", "--help"]]
  where
    child = info (pure () <**> helper) (progDesc "DISTINCT_CHILD_MARKER")
    spec = info ((helper <*> pure ()) <|> hsubparser (command "run" child))
      (progDesc "ROOT_ONLY_MARKER")
    check args = case execParserPure (prefs helpShowGlobals) spec args of
      Failure failure ->
        let (message, code) = renderFailure failure "issue461"
        in [(show args ++ " exits successfully", code == ExitSuccess)
           ,(show args ++ " selects distinct child", "DISTINCT_CHILD_MARKER" `isInfixOf` message)
           ,(show args ++ " omits root description", not ("ROOT_ONLY_MARKER" `isInfixOf` message))]
      _ -> [(show args ++ " expected child help", False)]
