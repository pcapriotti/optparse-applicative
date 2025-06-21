{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PatternGuards #-}

module Options.Applicative.Builder.Completer
  ( Completer
  , mkCompleter
  , listIOCompleter
  , listCompleter
  , bashCompleter

  , requote
  ) where

import Control.Applicative
import Prelude
import Control.Exception (IOException, try)
import System.Process (readProcess)

import Options.Applicative.Types
import qualified "os-string" System.OsString as OsString
import "os-string" System.OsString (OsString, osstr)
import qualified Data.Text as Strict
import Options.Applicative.Help (osStringToStrictText)

-- | Create a 'Completer' from an IO action
listIOCompleter :: IO [Strict.Text] -> Completer
listIOCompleter ss = Completer $ \s ->
  filter (Strict.isPrefixOf (osStringToStrictText s)) <$> ss

-- | Create a 'Completer' from a constant
-- list of strings.
listCompleter :: [Strict.Text] -> Completer
listCompleter = listIOCompleter . pure

-- | Run a compgen completion action.
--
-- Common actions include @file@ and
-- @directory@. See
-- <http://www.gnu.org/software/bash/manual/html_node/Programmable-Completion-Builtins.html#Programmable-Completion-Builtins>
-- for a complete list.
bashCompleter :: OsString -> Completer
bashCompleter action = Completer $ \word -> do
  cmd <- OsString.decodeUtf $  OsString.intercalate [osstr| |] [[osstr|compgen|], [osstr|-A|], action, [osstr|--|], requote word]
  result <- tryIO $ readProcess "bash" ["-c", cmd] ""
  return . (Strict.lines) . either (const Strict.empty) (Strict.pack) $ result

tryIO :: IO a -> IO (Either IOException a)
tryIO = try

-- | Strongly quote the string we pass to compgen.
--
-- We need to do this so bash doesn't expand out any ~ or other
-- chars we want to complete on, or emit an end of line error
-- when seeking the close to the quote.
requote :: OsString -> OsString
requote s =
  let
    -- Bash doesn't appear to allow "mixed" escaping
    -- in bash completions. So we don't have to really
    -- worry about people swapping between strong and
    -- weak quotes.
    
    unescaped =
       case OsString.uncons s of
        -- It's already strongly quoted, so we
        -- can use it mostly as is, but we must
        -- ensure it's closed off at the end and
        -- there's no single quotes in the
        -- middle which might confuse bash.
          Just (c, rs)
            | c  == OsString.unsafeFromChar '\'' -> unescapeN rs
        -- We're weakly quoted.
          Just (c, rs)
            | c == OsString.unsafeFromChar '"'-> unescapeD rs
        -- We're not quoted at all.
        -- We need to unescape some characters like
        -- spaces and quotation marks.
          _ -> unescapeU s
  in
   strong unescaped

  where
    strong ss = OsString.unsafeFromChar '\'' `OsString.cons` OsString.foldr go [osstr|'|] ss
      where
        -- If there's a single quote inside the
        -- command: exit from the strong quote and
        -- emit it the quote escaped, then resume.
        -- go '\'' t = "'\\''" ++ t
        -- go h t    = h : t
        go h t =
          if h == OsString.unsafeFromChar '\''
          then [osstr|'\''|] <> t
          else h `OsString.cons` t


    -- Unescape a strongly quoted string
    -- We have two recursive functions, as we
    -- can enter and exit the strong escaping.
    unescapeN = OsString.pack . goX
      where
        goX v = case OsString.uncons v of
          Just (x, xs)
            | x == OsString.unsafeFromChar '\''
            -> goN xs
          Just (x, xs) -> x : goX xs
          Nothing -> []

        goN v = case OsString.uncons v of
          Just (x, xs)
            | x == OsString.unsafeFromChar '\\'
            , Just (x', xs') <- OsString.uncons xs
            , x' == OsString.unsafeFromChar '\''
            -> x' : goN xs'
          Just (x, xs)
            | x == OsString.unsafeFromChar '\''
            -> goX xs
          Just (x, xs) -> x : goN xs
          Nothing -> []

    -- Unescape an unquoted string
    unescapeU = OsString.pack . goX
      where
        goX v = case OsString.uncons v of
          Nothing -> []
          Just (c1, xs)
            | c1 == OsString.unsafeFromChar '\\'
            , Just (x, xs') <- OsString.uncons xs
            -> x : goX xs'
          Just (x, xs) -> x : goX xs

    -- Unescape a weakly quoted string
    unescapeD = OsString.pack . goX
      where
        goX v = case OsString.uncons v of
          -- Reached an escape character
          Just (x, xs)
            | x == OsString.unsafeFromChar '\\'
            , Just (x', xs') <- OsString.uncons xs ->
                -- If it's true escapable, strip the
                -- slashes, as we're going to strong
                -- escape instead.
                if x'
                  `OsString.elem` [osstr||$`"|]
                  then x' : goX xs'
                  else x : x' : goX xs'
          -- We've ended quoted section, so we
          -- don't recurse on goX, it's done.
          Just (x, xs)
            | x == OsString.unsafeFromChar '"' ->
                OsString.unpack xs
          -- Not done, but not a special character
          -- just continue the fold.
          Just (x, xs) -> x : goX xs
          Nothing -> []
