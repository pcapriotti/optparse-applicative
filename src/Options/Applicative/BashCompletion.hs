{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
-- | You don't need to import this module to enable bash completion.
--
-- See
-- <http://github.com/pcapriotti/optparse-applicative/wiki/Bash-Completion the wiki>
-- for more information on bash completion.
module Options.Applicative.BashCompletion
  ( bashCompletionParser,

    bashCompletionScript,
    fishCompletionScript,
    zshCompletionScript,
  ) where

import Control.Applicative
import Prelude
import Data.Foldable ( asum )
import Data.Maybe ( fromMaybe, listToMaybe )

import Options.Applicative.Builder
import Options.Applicative.Common
import Options.Applicative.Internal
import Options.Applicative.Types
import Options.Applicative.Help.Pretty
import Options.Applicative.Help.Chunk
import "os-string" System.OsString (OsString, osstr)
import qualified "os-string" System.OsString as OsString
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text as Strict

-- | Provide basic or rich command completions
data Richness
  = Standard
  -- ^ Add no help descriptions to the completions
  | Enriched Int Int
  -- ^ Include tab separated description for options
  --   and commands when available.
  --   Takes option description length and command
  --   description length.
  deriving (Eq, Ord, Show)

bashCompletionParser :: ParserInfo a -> ParserPrefs -> Parser CompletionResult
bashCompletionParser pinfo pprefs = complParser
  where
    returnCompletions :: (OsString -> IO [Strict.Text]) -> CompletionResult
    returnCompletions opts =
      CompletionResult $
        \progn -> Strict.unlines <$> opts progn

    scriptRequest :: (OsString -> Strict.Text) -> CompletionResult
    scriptRequest =
      CompletionResult . fmap pure

    complParser :: Parser CompletionResult
    complParser = asum
      [ returnCompletions <$>
        (  let a = bashCompletionQuery pinfo pprefs in a
        -- To get rich completions, one just needs the first
        -- command. To customise the lengths, use either of
        -- the `desc-length` options.
        -- zsh commands can go on a single line, so they might
        -- want to be longer.
        <$> ( flag' Enriched (long [osstr|bash-completion-enriched|] `mappend` internal)
                <*> option auto (long [osstr|bash-completion-option-desc-length|] `mappend` internal `mappend` value 40)
                <*> option auto (long [osstr|bash-completion-command-desc-length|] `mappend` internal `mappend` value 40)
          <|> pure Standard
          )
        <*> (many . osStrOption) (long [osstr|bash-completion-word|]
                                  `mappend` internal)
        <*> option auto (long [osstr|bash-completion-index|] `mappend` internal) )

      , scriptRequest . bashCompletionScript <$>
            osStrOption (long [osstr|bash-completion-script|] `mappend` internal)
      , scriptRequest . fishCompletionScript <$>
            osStrOption (long [osstr|fish-completion-script|] `mappend` internal)
      , scriptRequest . zshCompletionScript <$>
            osStrOption (long [osstr|zsh-completion-script|] `mappend` internal)
      ]

bashCompletionQuery :: ParserInfo a -> ParserPrefs -> Richness -> [OsString] -> Int -> OsString -> IO [Strict.Text]
bashCompletionQuery pinfo pprefs richness ws i _ = case runCompletion compl pprefs of
  Just (Left (SomeParser p, a))
    -> list_options a p
  Just (Right c)
    -> run_completer c
  Nothing
    -> return []
  where
    compl = runParserInfo pinfo (drop 1 ws')

    list_options :: ArgPolicy -> Parser a -> IO [Strict.Text]
    list_options a
      = fmap concat
      . sequence
      . mapParser (opt_completions a)

    --
    -- Prior to 0.14 there was a subtle bug which would
    -- mean that completions from positional arguments
    -- further into the parse would be shown.
    --
    -- We therefore now check to see that
    -- hinfoUnreachableArgs is off before running the
    -- completion for position arguments.
    --
    -- For options and flags, ensure that the user
    -- hasn't disabled them with `--`.
    opt_completions :: ArgPolicy -> ArgumentReachability -> Option a -> IO [Strict.Text]
    opt_completions argPolicy reachability opt = case optMain opt of
      OptReader ns _ _
         | argPolicy /= AllPositionals
        -> return . add_opt_help opt $ show_names ns
         | otherwise
        -> return []
      FlagReader ns _
         | argPolicy /= AllPositionals
        -> return . add_opt_help opt $ show_names ns
         | otherwise
        -> return []
      ArgReader rdr
         | argumentIsUnreachable reachability
        -> return []
         | otherwise
        -> run_completer (crCompleter rdr)
      CmdReader _ ns
         | argumentIsUnreachable reachability
        -> return []
         | otherwise
        -> return . with_cmd_help $ filter (is_completion . fst) ns

    -- When doing enriched completions, add any help specified
    -- to the completion variables (tab separated).
    add_opt_help :: Option a -> [OsString] -> [Strict.Text]
    add_opt_help opt = case richness of
      Standard ->
        fmap osStringToStrictText
      Enriched len _ ->
        fmap $ \o ->
          let h = unChunk $ optHelp opt
              o' = osStringToLazyText o
          in  maybe (osStringToStrictText o) (\h' -> Lazy.toStrict (o' <> "\t" <> render_line len h')) h

    -- When doing enriched completions, add the command description
   -- to the completion variables (tab separated).
    with_cmd_help :: [(OsString, ParserInfo a)] -> [Strict.Text]
    with_cmd_help =
      case richness of
        Standard ->
          fmap (osStringToStrictText . fst)
        Enriched _ len ->
          fmap $ \(cmd, cmdInfo) ->
            let h = unChunk (infoProgDesc cmdInfo)
                cmd' = osStringToLazyText cmd
            in  maybe (osStringToStrictText cmd) (\h' -> Lazy.toStrict ((cmd' `Lazy.snoc` '\t') <> render_line len h')) h

    show_names :: [OptName] -> [OsString]
    show_names = filter is_completion . map showOption

    -- We only want to show a single line in the completion results description.
    -- If there was a line break, it would come across as a different completion
    -- possibility.
    render_line :: Int -> Doc -> Lazy.Text
    render_line len doc = case Lazy.lines (prettyLazyText 1 len doc) of
      [] -> Lazy.empty
      [x] -> x
      x : _ -> x <> "..."

    run_completer :: Completer -> IO [Strict.Text]
    run_completer c = runCompleter c (fromMaybe OsString.empty (listToMaybe ws''))

    (ws', ws'') = splitAt i ws

    is_completion :: OsString -> Bool
    is_completion =
      case ws'' of
        w:_ -> OsString.isPrefixOf w
        _ -> const True

-- | Generated bash shell completion script
bashCompletionScript :: OsString -> OsString -> Strict.Text
bashCompletionScript prog progn = Strict.unlines
  [ "_" <> osStringToStrictText progn <> "()"
  , "{"
  , "    local CMDLINE"
  , "    local IFS=$'\\n'"
  , "    CMDLINE=(--bash-completion-index $COMP_CWORD)"
  , ""
  , "    for arg in ${COMP_WORDS[@]}; do"
  , "        CMDLINE=(${CMDLINE[@]} --bash-completion-word $arg)"
  , "    done"
  , ""
  , "    COMPREPLY=( $(" <> prog' <> " \"${CMDLINE[@]}\") )"
  , "}"
  , ""
  , "complete -o filenames -F _" <> progn' <> " " <> progn' ]
  where 
    progn' = osStringToStrictText progn
    prog' = osStringToStrictText prog

{-
/Note/: Fish Shell

Derived from Drezil's post in #169.

@
commandline
-c or --cut-at-cursor only print selection up until the current cursor position
-o or --tokenize tokenize the selection and print one string-type token per line
@

We tokenize so that the call to count (and hence --bash-completion-index)
gets the right number use cut-at-curstor to not bother sending anything
after the cursor position, which allows for completion of the middle of
words.

Tab characters separate items from descriptions.
-}

-- | Generated fish shell completion script 
fishCompletionScript :: OsString -> OsString -> Strict.Text
fishCompletionScript prog progn = Strict.unlines
  [ " function _" <> osStringToStrictText progn
  , "    set -l cl (commandline --tokenize --current-process)"
  , "    # Hack around fish issue #3934"
  , "    set -l cn (commandline --tokenize --cut-at-cursor --current-process)"
  , "    set -l cn (count $cn)"
  , "    set -l tmpline --bash-completion-enriched --bash-completion-index $cn"
  , "    for arg in $cl"
  , "      set tmpline $tmpline --bash-completion-word $arg"
  , "    end"
  , "    for opt in (" <> osStringToStrictText prog <> " $tmpline)"
  , "      if test -d $opt"
  , "        echo -E \"$opt/\""
  , "      else"
  , "        echo -E \"$opt\""
  , "      end"
  , "    end"
  , "end"
  , ""
  , "complete --no-files --command " <> osStringToStrictText progn <> " --arguments '(_" <> osStringToStrictText progn <>  ")'"
  ]

-- | Generated zsh shell completion script
zshCompletionScript :: OsString -> OsString -> Strict.Text
zshCompletionScript prog progn = Strict.unlines
  [ "#compdef " <> osStringToStrictText progn
  , ""
  , "local request"
  , "local completions"
  , "local word"
  , "local index=$((CURRENT - 1))"
  , ""
  , "request=(--bash-completion-enriched --bash-completion-index $index)"
  , "for arg in ${words[@]}; do"
  , "  request=(${request[@]} --bash-completion-word $arg)"
  , "done"
  , ""
  , "IFS=$'\\n' completions=($( " <> osStringToStrictText prog <> " \"${request[@]}\" ))"
  , ""
  , "for word in $completions; do"
  , "  local -a parts"
  , ""
  , "  # Split the line at a tab if there is one."
  , "  IFS=$'\\t' parts=($( echo $word ))"
  , ""
  , "  if [[ -n $parts[2] ]]; then"
  , "     if [[ $word[1] == \"-\" ]]; then"
  , "       local desc=(\"$parts[1] ($parts[2])\")"
  , "       compadd -d desc -- $parts[1]"
  , "     else"
  , "       local desc=($(print -f  \"%-019s -- %s\" $parts[1] $parts[2]))"
  , "       compadd -l -d desc -- $parts[1]"
  , "     fi"
  , "  else"
  , "    compadd -f -- $word"
  , "  fi"
  , "done"
  ]
