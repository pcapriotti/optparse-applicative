module Options.Applicative.Builder.Completer
  ( listIOCompleter
  , listCompleter
  , fileCompleter
  , dirCompleter
  ) where

import Control.Applicative
import Control.Monad
import Data.List
import Options.Applicative.Types
import System.Directory

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
