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
import System.Environment (lookupEnv)

import Options.Applicative.Types

-- | Create a 'Completer' from an IO action
listIOCompleter :: IO [String] -> Completer
listIOCompleter ss = Completer $ \s ->
  filter (isPrefixOf s) <$> ss

-- | Create a 'Completer' from a constant
-- list of strings.
listCompleter :: [String] -> Completer
listCompleter = listIOCompleter . pure

-- | Run a compgen completion action.
--
-- Common actions include @file@ and
-- @directory@. See
-- <http://www.gnu.org/software/bash/manual/html_node/Programmable-Completion-Builtins.html#Programmable-Completion-Builtins>
-- for a complete list.
bashCompleter :: String -> Completer
bashCompleter action = Completer $ \word -> do
  let cmd = unwords ["compgen", "-A", action, "--", requote word]
  bash <- getBash
  result <- tryIO $ readProcess bash ["-c", cmd] ""
  return . lines . either (const []) id $ result

-- | Determines the bash executable. Ideally we'd invoke the same bash that
-- is currently active. If $SHELL does not seem to be set to a bash executable
-- we don't assume $SHELL is bash and we take bash from the $PATH.
-- This fixes file completion in cases where a virtual environment with a
-- non-interactive bash is loaded with direnv, nix-shell or similar.
getBash :: IO String
getBash = do
  shellEnv <- lookupEnv "SHELL"
  pure (case shellEnv of
      Just exe | "/bash" `isSuffixOf` exe -> exe
      _ -> "bash"
    )

tryIO :: IO a -> IO (Either IOException a)
tryIO = try

-- | Strongly quote the string we pass to compgen.
--
-- We need to do this so bash doesn't expand out any ~ or other
-- chars we want to complete on, or emit an end of line error
-- when seeking the close to the quote.
requote :: String -> String
requote s =
  let
    -- Bash doesn't appear to allow "mixed" escaping
    -- in bash completions. So we don't have to really
    -- worry about people swapping between strong and
    -- weak quotes.
    unescaped =
      case s of
        -- It's already strongly quoted, so we
        -- can use it mostly as is, but we must
        -- ensure it's closed off at the end and
        -- there's no single quotes in the
        -- middle which might confuse bash.
        ('\'': rs) -> unescapeN rs

        -- We're weakly quoted.
        ('"': rs)  -> unescapeD rs

        -- We're not quoted at all.
        -- We need to unescape some characters like
        -- spaces and quotation marks.
        elsewise   -> unescapeU elsewise
  in
    strong unescaped

  where
    strong ss = '\'' : foldr go "'" ss
      where
        -- If there's a single quote inside the
        -- command: exit from the strong quote and
        -- emit it the quote escaped, then resume.
        go '\'' t = "'\\''" ++ t
        go h t    = h : t

    -- Unescape a strongly quoted string
    -- We have two recursive functions, as we
    -- can enter and exit the strong escaping.
    unescapeN = goX
      where
        goX ('\'' : xs) = goN xs
        goX (x : xs) = x : goX xs
        goX [] = []

        goN ('\\' : '\'' : xs) = '\'' : goN xs
        goN ('\'' : xs) = goX xs
        goN (x : xs) = x : goN xs
        goN [] = []

    -- Unescape an unquoted string
    unescapeU = goX
      where
        goX [] = []
        goX ('\\' : x : xs) = x : goX xs
        goX (x : xs) = x : goX xs

    -- Unescape a weakly quoted string
    unescapeD = goX
      where
        -- Reached an escape character
        goX ('\\' : x : xs)
          -- If it's true escapable, strip the
          -- slashes, as we're going to strong
          -- escape instead.
          | x `elem` "$`\"\\\n" = x : goX xs
          | otherwise = '\\' : x : goX xs
        -- We've ended quoted section, so we
        -- don't recurse on goX, it's done.
        goX ('"' : xs)
          = xs
        -- Not done, but not a special character
        -- just continue the fold.
        goX (x : xs)
          = x : goX xs
        goX []
          = []
