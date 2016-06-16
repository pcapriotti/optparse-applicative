-- | You don't need to import this module to enable bash completion.
--
-- See
-- <http://github.com/pcapriotti/optparse-applicative/wiki/Bash-Completion the wiki>
-- for more information on bash completion.
module Options.Applicative.BashCompletion
  ( bashCompletionParser
  ) where

import Control.Applicative
import Prelude
import Data.Foldable (asum)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe, listToMaybe)

import Options.Applicative.Builder
import Options.Applicative.Common
import Options.Applicative.Internal
import Options.Applicative.Types

bashCompletionParser :: ParserInfo a -> ParserPrefs -> Parser CompletionResult
bashCompletionParser pinfo pprefs = complParser
  where
    failure opts = CompletionResult
      { execCompletion = \progn -> unlines <$> opts progn }

    complParser = asum
      [ failure <$>
        (   bashCompletionQuery pinfo pprefs
        <$> (many . strOption) (long "bash-completion-word"
                                  `mappend` internal)
        <*> option auto (long "bash-completion-index" `mappend` internal) )
      , failure <$>
          (bashCompletionScript <$>
            strOption (long "bash-completion-script" `mappend` internal)) ]

bashCompletionQuery :: ParserInfo a -> ParserPrefs -> [String] -> Int -> String -> IO [String]
bashCompletionQuery pinfo pprefs ws i _ = case runCompletion compl pprefs of
  Just (Left (SomeParser p)) -> list_options p
  Just (Right c)             -> run_completer c
  _                          -> return []
  where
    list_options =
        fmap concat
      . sequence
      . mapParser (const opt_completions)

    opt_completions opt = case optMain opt of
      OptReader ns _ _ -> return $ show_names ns
      FlagReader ns _  -> return $ show_names ns
      ArgReader rdr    -> run_completer (crCompleter rdr)
      CmdReader _ ns _ -> return $ filter_names ns

    show_name :: OptName -> String
    show_name (OptShort c) = '-':[c]
    show_name (OptLong name) = "--" ++ name

    show_names :: [OptName] -> [String]
    show_names = filter_names . map show_name

    filter_names :: [String] -> [String]
    filter_names = filter is_completion

    run_completer :: Completer -> IO [String]
    run_completer c = runCompleter c (fromMaybe "" (listToMaybe ws''))

    (ws', ws'') = splitAt i ws

    is_completion :: String -> Bool
    is_completion =
      case ws'' of
        w:_ -> isPrefixOf w
        _ -> const True

    compl = runParserInfo pinfo (drop 1 ws')

bashCompletionScript :: String -> String -> IO [String]
bashCompletionScript prog progn = return
  [ "_" ++ progn ++ "()"
  , "{"
  , "    local cmdline"
  , "    local IFS=$'\n'"
  , "    CMDLINE=(--bash-completion-index $COMP_CWORD)"
  , ""
  , "    for arg in ${COMP_WORDS[@]}; do"
  , "        CMDLINE=(${CMDLINE[@]} --bash-completion-word $arg)"
  , "    done"
  , ""
  , "    COMPREPLY=( $(" ++ prog ++ " \"${CMDLINE[@]}\") )"
  , "}"
  , ""
  , "complete -o filenames -F _" ++ progn ++ " " ++ progn ]
