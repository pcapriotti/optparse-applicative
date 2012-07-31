module Options.Applicative.BashCompletion
  ( bashCompletionParser
  ) where

import Control.Applicative
import System.Exit

import Options.Applicative.Builder
import Options.Applicative.Common
import Options.Applicative.Internal
import Options.Applicative.Types

bashCompletionParser :: Parser a -> Parser ParserFailure
bashCompletionParser parser = complParser
  where
    result ws i = failure $ case runCompletion compl ws i of
      Nothing -> []
      Just p  -> list_options p

    failure opts = ParserFailure
      { errMessage = \_ -> unlines opts
      , errExitCode = ExitSuccess }
    compl = runParserWith pure parser

    complParser = result
      <$> (many . strOption) (long "bash-completion-word")
      <*> option (long "bash-completion-index")

    list_options = concat . mapParser (\_ -> map show_name . optionNames . optMain)
    show_name (OptShort c) = '-':[c]
    show_name (OptLong name) = "--" ++ name

bashCompletionQuery :: Parser a -> [String] -> Int -> [String]
bashCompletionQuery _ _ _ = []
