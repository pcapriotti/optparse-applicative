module Options.Applicative.BashCompletion
  ( bashCompletionParser
  ) where

import Control.Applicative
import Options.Applicative.Types
import System.Exit

bashCompletionParser :: Parser a -> Parser ParserFailure
bashCompletionParser parser = pure ParserFailure
  { errMessage = \_ -> "bash completion"
  , errExitCode = ExitSuccess }

bashCompletionQuery :: [String] -> Int -> [String]
bashCompletionQuery _ _ = []
