{-# LANGUAGE PatternGuards #-}
module Options.Applicative.BashCompletion
  ( bashCompletionParser
  ) where

import Control.Applicative
import Data.Foldable (asum)
import Data.List
import System.Exit

import Options.Applicative.Builder
import Options.Applicative.Common
import Options.Applicative.Internal
import Options.Applicative.Types

bashCompletionParser :: Parser a -> Parser ParserFailure
bashCompletionParser parser = complParser
  where
    failure opts = ParserFailure
      { errMessage = \_ -> unlines opts
      , errExitCode = ExitSuccess }

    complParser = asum
      [ failure <$>
        (   bashCompletionQuery parser
        <$> (many . strOption) (long "bash-completion-word")
        <*> option (long "bash-completion-index") )
      , ParserFailure
          <$> (bashCompletionScript <$>
                strOption (long "bash-completion-script"))
          <*> pure ExitSuccess ]

bashCompletionQuery :: Parser a -> [String] -> Int -> [String]
bashCompletionQuery parser ws i = case runCompletion compl parser of
  (Left ComplExit, SomeParser p, _) -> list_options p
  _ -> []
  where
    list_options = filter is_completion
                 . concat
                 . mapParser (\_ -> opt_names)

    opt_names opt = case optMain opt of
      OptReader ns _  -> map show_name ns
      FlagReader ns _ -> map show_name ns
      ArgReader _     -> []
      CmdReader ns _  -> ns

    show_name (OptShort c) = '-':[c]
    show_name (OptLong name) = "--" ++ name

    (ws', ws'') = splitAt i ws

    is_completion
      | (w:_) <- ws'' = isPrefixOf w
      | otherwise     = const True

    compl = do
      setParser Nothing parser
      runParserFully parser (drop 1 ws')

bashCompletionScript :: String -> String -> String
bashCompletionScript prog progn = unlines
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
  , "complete -F _" ++ progn ++ " " ++ progn ]
