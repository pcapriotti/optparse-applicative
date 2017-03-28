module Options.Applicative.Builder.Completer
  ( Completer
  , mkCompleter
  , listIOCompleter
  , listCompleter
  , bashCompleter
  ) where

import Control.Applicative
import Prelude
import Control.Exception (IOException, try)
import Data.List (isPrefixOf, isSuffixOf)
import System.Process (readProcess)

import Options.Applicative.Types

listIOCompleter :: IO [String] -> Completer
listIOCompleter ss = Completer $ \s ->
  filter (isPrefixOf s) <$> ss

listCompleter :: [String] -> Completer
listCompleter = listIOCompleter . pure

bashCompleter :: String -> Completer
bashCompleter action = Completer $ \word -> do
  let cmd = unwords ["compgen", "-A", action, "--", quote word]
  result <- tryIO $ readProcess "bash" ["-c", cmd] ""
  return . lines . either (const []) id $ result

tryIO :: IO a -> IO (Either IOException a)
tryIO = try

-- | Strongly quote the string we pass to compgen.
--
-- We need to do this so bash doesn't expand out any ~ or other
-- chars we want to complete on, or emit an end of line error
-- when seeking the close to the quote.
quote :: String -> String
quote s =
  case s of
    -- It's already strongly quoted, so we
    -- can use it mostly as is, but we must
    -- ensure it's closed off at the end and
    -- there's no single quotes in the
    -- middle which might confuse bash.
    ('\'' : rs) | isSuffixOf "'" rs
               -> '\'' : foldr go [] rs
                | otherwise
               -> '\'' : foldr go "'" rs

    -- We're not strongly quoted. Make it so.
    -- We could be either weakly quoted or not
    -- quoted at all, but it looks like this
    -- actually works well for us either way.
    elsewise   -> '\'' : foldr go "'" elsewise

  where
    -- If there's a single quote inside the
    -- command: exit from the strong quote and
    -- emit it the quote escaped, then resume.
    go '\'' t = "'\\''" ++ t
    go h t    = h : t
