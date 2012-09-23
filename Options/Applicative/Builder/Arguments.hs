module Options.Applicative.Builder.Arguments
  ( argument
  , arguments
  ) where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Monoid
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
arguments :: (String -> Maybe a) -> Mod ArgumentFields [a] -> Parser [a]
arguments p m = set_default <$> fromM args
  where
    Mod f (DefaultProp def sdef) g = m
    show_def = sdef <*> def

    p' ('-':_) = Nothing
    p' s = p s

    props = mkProps mempty g
    props' = (mkProps mempty g) { propShowDefault = show_def }

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
    arg' = liftOpt (Option (ArgReader (CReader compl p')) props')

    ddash = argument (guard . (== "--")) internal

    ArgumentFields compl = f (ArgumentFields mempty)
