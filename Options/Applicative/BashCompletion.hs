module Options.Applicative.BashCompletion
  ( bashCompletionParser
  ) where

import Control.Applicative ((<$>), (<*>), many)
import Data.Foldable (asum)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe, listToMaybe)

import Options.Applicative.Builder
import Options.Applicative.Common
import Options.Applicative.Internal
import Options.Applicative.Types

bashCompletionParser :: Parser a -> ParserPrefs -> Parser CompletionResult
bashCompletionParser parser pprefs = complParser
  where
    failure opts = CompletionResult
      { execCompletion = \progn -> unlines <$> opts progn }

    complParser = asum
      [ failure <$>
        (   bashCompletionQuery parser pprefs
        <$> (many . strOption) (long "bash-completion-word"
                                  `mappend` internal)
        <*> option (long "bash-completion-index" `mappend` internal) )
      , failure <$>
          (bashCompletionScript <$>
            strOption (long "bash-completion-script" `mappend` internal)) ]

bashCompletionQuery :: Parser a -> ParserPrefs -> [String] -> Int -> String -> IO [String]
bashCompletionQuery parser pprefs ws i _ = case runCompletion compl pprefs of
  Just (Left (SomeParser p)) -> list_options p
  Just (Right c)             -> run_completer c
  _                          -> return []
  where
    list_options =
        fmap concat
      . sequence
      . mapParser (\_ -> opt_completions)

    opt_completions opt = case optMain opt of
      OptReader ns _ _ -> return $ show_names ns
      FlagReader ns _  -> return $ show_names ns
      ArgReader rdr    -> run_completer (crCompleter rdr)
      CmdReader ns _   -> return $ filter_names ns

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

    compl = do
      setParser Nothing parser
      runParserFully parser (drop 1 ws')

bashCompletionScript :: String -> String -> IO [String]
bashCompletionScript prog progn = return
  [ "_" ++ progn ++ "()"
  , "{"
  , "    local cmdline"
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
