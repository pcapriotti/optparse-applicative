module Options.Applicative.Builder.Completer
  ( listIOCompleter
  , listCompleter
  , fileCompleter
  , dirCompleter
  , bashCompleter
  ) where

import Control.Applicative
import Control.Monad
import Data.List
import Options.Applicative.Types
import System.Directory
import System.Process

listIOCompleter :: IO [String] -> Completer
listIOCompleter ss = Completer $ \s ->
  filter (isPrefixOf s) <$> ss

listCompleter :: [String] -> Completer
listCompleter = listIOCompleter . pure

fileCompleter :: Completer
fileCompleter = listIOCompleter $
  getDirectoryContents "."

dirCompleter :: Completer
dirCompleter = listIOCompleter $ do
  files <- getDirectoryContents "."
  filterM doesDirectoryExist files

bashCompleter :: String -> Completer
bashCompleter action = Completer $ \word -> do
  let cmd = unwords ["compgen", "-A", action, word]
  result <- readProcess "bash" ["-c", cmd] ""
  return $ lines result
