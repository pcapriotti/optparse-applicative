module Options.Applicative.Builder.Arguments
  ( argument
  , argument'
  , arguments
  , arguments1
  ) where

import Control.Applicative ((<$>), pure, (<*>), optional, (<|>), (*>))
import Control.Monad (guard)
import Data.Maybe (fromMaybe)
import Data.Monoid (mempty)

import Options.Applicative.Builder.Internal
import Options.Applicative.Common
import Options.Applicative.Types

skipOpts :: (String -> Maybe a) -> String -> Maybe a
skipOpts _ ('-':_) = Nothing
skipOpts rdr s = rdr s

-- | Builder for an argument parser.
argument' :: (String -> Maybe a) -> Mod ArgumentFields a -> Parser a
argument' p (Mod f d g) = mkParser d g (ArgReader rdr)
  where
    ArgumentFields compl = f (ArgumentFields mempty)
    rdr = CReader compl p

-- | Builder for an argument parser ignoring arguments starting with '-'.
argument :: (String -> Maybe a) -> Mod ArgumentFields a -> Parser a
argument p = argument' (skipOpts p)

-- | Builder for an argument list parser. All arguments are collected and
-- returned as a list.
--
-- Note that arguments starting with @'-'@ are ignored.
--
-- This parser accepts a special argument: @--@. When a @--@ is found on the
-- command line, all following arguments are included in the result, even if
-- they start with @'-'@.
arguments :: (String -> Maybe a) -> Mod ArgumentFields [a] -> Parser [a]
arguments = arguments_ True

-- | Like `arguments`, but require at least one argument.
arguments1 :: (String -> Maybe a) -> Mod ArgumentFields [a] -> Parser [a]
arguments1 = arguments_ False

-- | Builder for an argument list parser. All arguments are collected and
-- returned as a list.
--
-- Note that arguments starting with @'-'@ are ignored.
--
-- This parser accepts a special argument: @--@. When a @--@ is found on the
-- command line, all following arguments are included in the result, even if
-- they start with @'-'@.
arguments_ :: Bool -> (String -> Maybe a) -> Mod ArgumentFields [a] -> Parser [a]
arguments_ allow_empty p m = set_default <$> fromM args1
  where
    Mod f (DefaultProp def sdef) g = m
    show_def = sdef <*> def

    props = mkProps mempty g
    props' = (mkProps mempty g) { propShowDefault = show_def }

    args1 | allow_empty = args
          | otherwise = do
      mx <- oneM arg_or_ddash
      case mx of
        Nothing -> someM arg
        Just x  -> (x:) <$> args
    args = do
      mx <- oneM $ optional arg_or_ddash
      case mx of
        Nothing       -> return []
        Just Nothing  -> manyM arg
        Just (Just x) -> (x:) <$> args
    arg_or_ddash = (Just <$> arg') <|> (ddash *> pure Nothing)
    set_default [] = fromMaybe [] def
    set_default xs = xs

    arg = liftOpt (Option (ArgReader (CReader compl p)) props)
    arg' = liftOpt (Option (ArgReader (CReader compl (skipOpts p))) props')

    ddash = argument' (guard . (== "--")) internal

    ArgumentFields compl = f (ArgumentFields mempty)
