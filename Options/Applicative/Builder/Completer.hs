module Options.Applicative.Builder.Completer
  ( listIOCompleter
  , listCompleter
  , bashCompleter
  ) where

import Control.Applicative
import Control.Exception (IOException, try)
import Data.List
import Options.Applicative.Types
import System.Process

listIOCompleter :: IO [String] -> Completer
listIOCompleter ss = Completer $ \s ->
  filter (isPrefixOf s) <$> ss

listCompleter :: [String] -> Completer
listCompleter = listIOCompleter . pure

bashCompleter :: String -> Completer
bashCompleter action = Completer $ \word -> do
  let cmd = unwords ["compgen", "-A", action, word]

  result <- tryIO $ readProcess "bash" ["-c", cmd] ""
  return . lines . either (const []) id $ result

tryIO :: IO a -> IO (Either IOException a)
tryIO = try
