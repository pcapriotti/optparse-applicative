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
import Data.List ( isPrefixOf )
import Data.Maybe ( fromMaybe, listToMaybe )

import Options.Applicative.Builder
import Options.Applicative.Common
import Options.Applicative.Internal
import Options.Applicative.Types
import Options.Applicative.Help.Pretty
import Options.Applicative.Help.Chunk

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
    returnCompletions opts =
      CompletionResult $
        \progn -> unlines <$> opts progn

    scriptRequest =
      CompletionResult . fmap pure

    complParser = asum
      [ returnCompletions <$>
        (  bashCompletionQuery pinfo pprefs
        -- To get rich completions, one just needs the first
        -- command. To customise the lengths, use either of
        -- the `desc-length` options.
        -- zsh commands can go on a single line, so they might
        -- want to be longer.
        <$> ( flag' Enriched (long "bash-completion-enriched" `mappend` internal)
                <*> option auto (long "bash-completion-option-desc-length" `mappend` internal `mappend` value 40)
                <*> option auto (long "bash-completion-command-desc-length" `mappend` internal `mappend` value 40)
          <|> pure Standard
          )
        <*> (many . strOption) (long "bash-completion-word"
                                  `mappend` internal)
        <*> option auto (long "bash-completion-index" `mappend` internal) )

      , scriptRequest . bashCompletionScript <$>
            strOption (long "bash-completion-script" `mappend` internal)
      , scriptRequest . fishCompletionScript <$>
            strOption (long "fish-completion-script" `mappend` internal)
      , scriptRequest . zshCompletionScript <$>
            strOption (long "zsh-completion-script" `mappend` internal)
      ]

bashCompletionQuery :: ParserInfo a -> ParserPrefs -> Richness -> [String] -> Int -> String -> IO [String]
bashCompletionQuery pinfo pprefs richness ws i _ = case runCompletion compl pprefs of
  Just (Left (SomeParser p, a))
    -> render_items <$> list_options a p
  Just (Right c)
    -> render_items <$> run_completer c
  Nothing
    -> return []
  where
    compl = runParserInfo pinfo (drop 1 ws')

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
    opt_completions argPolicy reachability opt = case optMain opt of
      OptReader ns _ _
         | argPolicy /= AllPositionals
        -> return . fmap defaultCompletionItem . add_opt_help opt $ show_names ns
         | otherwise
        -> return []
      FlagReader ns _
         | argPolicy /= AllPositionals
        -> return . fmap defaultCompletionItem . add_opt_help opt $ show_names ns
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
        -> return . fmap defaultCompletionItem . with_cmd_help $ filter (is_completion . fst) ns

    -- When doing enriched completions, add any help specified
    -- to the completion variables (tab separated).
    add_opt_help :: Functor f => Option a -> f String -> f String
    add_opt_help opt = case richness of
      Standard ->
        id
      Enriched len _ ->
        fmap $ \o ->
          let h = unChunk $ optHelp opt
          in  maybe o (\h' -> o ++ "\t" ++ render_line len h') h

    -- When doing enriched completions, add the command description
    -- to the completion variables (tab separated).
    with_cmd_help :: Functor f => f (String, ParserInfo a) -> f String
    with_cmd_help =
      case richness of
        Standard ->
          fmap fst
        Enriched _ len ->
          fmap $ \(cmd, cmdInfo) ->
            let h = unChunk (infoProgDesc cmdInfo)
            in  maybe cmd (\h' -> cmd ++ "\t" ++ render_line len h') h

    show_names :: [OptName] -> [String]
    show_names = filter is_completion . map showOption

    -- We only want to show a single line in the completion results description.
    -- If there was a line break, it would come across as a different completion
    -- possibility.
    render_line :: Int -> Doc -> String
    render_line len doc = case lines (prettyString 1 len doc) of
      [] -> ""
      [x] -> x
      x : _ -> x ++ "..."

    run_completer :: Completer -> IO [CompletionItem]
    run_completer c = runCompleter c (fromMaybe "" (listToMaybe ws''))

    (ws', ws'') = splitAt i ws

    is_completion :: String -> Bool
    is_completion =
      case ws'' of
        w:_ -> isPrefixOf w
        _ -> const True

    render_items :: [CompletionItem] -> [String]
    render_items = concatMap render_item

    render_item :: CompletionItem -> [String]
    render_item CompletionItem { ciOptions = opts, ciValue = val } =
      [ "%addspace" | cioAddSpace opts ]
      ++ [ "%files" | cioFiles opts ]
      ++ ["%value", val]

-- | Generated bash shell completion script
bashCompletionScript :: String -> String -> String
bashCompletionScript prog progn = unlines
  -- compopt: see complete -o at https://www.gnu.org/software/bash/manual/html_node/Programmable-Completion-Builtins.html
  [ "_" ++ progn ++ "()"
  , "{"
  , "    local CMDLINE"
  , "    local value_mode=false"
  , "    local IFS=$'\\n'"
  , "    CMDLINE=(--bash-completion-index $COMP_CWORD)"
  , ""
  , "    for arg in ${COMP_WORDS[@]}; do"
  , "        CMDLINE=(${CMDLINE[@]} --bash-completion-word $arg)"
  , "    done"
  , ""
  , "    compopt -o nospace +o filenames"
  , "    COMPREPLY=()"
  , "    for ln in $(" ++ prog ++ " \"${CMDLINE[@]}\"); do"
  , "        if $value_mode; then"
  , "            COMPREPLY+=($ln)"
  , "            value_mode=false"
  , "        else"
  , "            case $ln in"
  , "                %value)"
  , "                    value_mode=true"
  , "                    ;;"
  , "                %addspace)"
  , "                    compopt +o nospace"
  , "                    ;;"
  , "                %files)"
  , "                    compopt -o filenames"
  , "                    ;;"
  , "            esac"
  , "        fi"
  , "    done"
  , "}"
  , ""
  , "complete -o filenames -F _" ++ progn ++ " " ++ progn ]

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
fishCompletionScript :: String -> String -> String
fishCompletionScript prog progn = unlines
  [ " function _" ++ progn
  , "    set -l cl (commandline --tokenize --current-process)"
  , "    # Hack around fish issue #3934"
  , "    set -l cn (commandline --tokenize --cut-at-cursor --current-process)"
  , "    set -l cn (count $cn)"
  , "    set -l tmpline --bash-completion-enriched --bash-completion-index $cn"
  , "    for arg in $cl"
  , "      set tmpline $tmpline --bash-completion-word $arg"
  , "    end"
  , "    set -l value_mode false"
  , "    for ln in (" ++ prog ++ " $tmpline)"
  , "      if $value_mode"
  , "        if test -d $ln"
  , "          echo -E \"$ln/\""
  , "        else"
  , "          echo -E \"$ln\""
  , "        end"
  , "        set value_mode false"
  , "      else"
  , "        switch $ln"
  , "          case '%value'"
  , "            set value_mode true"
    --         Ignore %addspace, because fish does not let us remove the end
    --         space. Dynamic control has not been implemented as of 2020, see
    --         https://github.com/fish-shell/fish-shell/issues/6928#issuecomment-618012509
  , "        end"
  , "      end"
  , "    end"
  , "end"
  , ""
  , "complete --no-files --command " ++ progn ++ " --arguments '(_"  ++ progn ++  ")'"
  ]

-- | Generated zsh shell completion script
zshCompletionScript :: String -> String -> String
zshCompletionScript prog progn = unlines
  -- compadd: http://zsh.sourceforge.net/Doc/Release/Completion-Widgets.html#Completion-Builtin-Commands
  [ "#compdef " ++ progn
  , ""
  , "local request"
  , "local completions"
  , "local word"
  , "local value_mode=false"
  , "local addspace=false"
  , "local files=false"
  , "local index=$((CURRENT - 1))"
  , ""
  , "request=(--bash-completion-enriched --bash-completion-index $index)"
  , "for arg in ${words[@]}; do"
  , "  request=(${request[@]} --bash-completion-word $arg)"
  , "done"
  , ""
  , "IFS=$'\\n' completionLines=($( " ++ prog ++ " \"${request[@]}\" ))"
  , ""
  , "for word in $completionLines; do"
  , "  if $value_mode; then"
  , "    local -a parts args"
  , ""
  , "    # Split the line at a tab if there is one."
  , "    IFS=$'\\t' parts=($( echo $word ))"
  , ""
  , "    if $addspace; then"
  , "      args+=( -S' ' )"
  , "    fi"
  , ""
  , "    if [[ -n $parts[2] ]]; then"
  , "       if [[ $word[1] == \"-\" ]]; then"
  , "         local desc=(\"$parts[1] ($parts[2])\")"
  , "         compadd $args -d desc -- $parts[1]"
  , "       else"
  , "         local desc=($(print -f  \"%-019s -- %s\" $parts[1] $parts[2]))"
  , "         compadd $args -l -d desc -- $parts[1]"
  , "       fi"
  , "    else"
  , "      compadd $args -f -- $word"
  , "    fi"
  , "    value_mode=false"
  , "    addspace=false"
  , "    files=false"
  , "  else"
  , "    case $word in"
  , "      %value)"
  , "        value_mode=true"
  , "        ;;"
  , "      %addspace)"
  , "        addspace=true"
  , "        ;;"
  , "      %files)"
  , "        files=true"
  , "        ;;"
  , "    esac"
  , "  fi"
  , "done"
  ]
