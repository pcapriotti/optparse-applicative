module Options.Applicative.Builder.Arguments
  ( argument
  , arguments
  , arguments1
  ) where

import Control.Applicative (many, some)
import Data.Monoid (mempty)

import Options.Applicative.Builder.Internal
import Options.Applicative.Common
import Options.Applicative.Types

-- | Builder for an argument parser.
argument :: (String -> Maybe a) -> Mod ArgumentFields a -> Parser a
argument p (Mod f d g) = mkParser d g (ArgReader rdr)
  where
    ArgumentFields compl = f (ArgumentFields mempty)
    rdr = CReader compl p

-- | Builder for an argument list parser. All arguments are collected and
-- returned as a list.
--
-- Note that arguments starting with @'-'@ are ignored.
--
-- This parser accepts a special argument: @--@. When a @--@ is found on the
-- command line, all following arguments are included in the result, even if
-- they start with @'-'@.
arguments :: (String -> Maybe a) -> Mod ArgumentFields a -> Parser [a]
arguments r m = many (argument r m)

-- | Like `arguments`, but require at least one argument.
arguments1 :: (String -> Maybe a) -> Mod ArgumentFields a -> Parser [a]
arguments1 r m = some (argument r m)
