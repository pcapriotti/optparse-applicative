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
    failure opts = ParserFailure
      { errMessage = \_ -> unlines opts
      , errExitCode = ExitSuccess }

    complParser = failure <$>
      (   bashCompletionQuery parser
      <$> (many . strOption) (long "bash-completion-word")
      <*> option (long "bash-completion-index") )

bashCompletionQuery :: Parser a -> [String] -> Int -> [String]
bashCompletionQuery parser ws i = case runCompletion compl ws i of
  Nothing -> []
  Just p  -> list_options p
  where
    list_options = concat . mapParser (\_ -> map show_name . optionNames . optMain)
    show_name (OptShort c) = '-':[c]
    show_name (OptLong name) = "--" ++ name

    compl = runParserWith pure parser
