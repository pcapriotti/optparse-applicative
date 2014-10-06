module Options.Applicative.Classes where

import Control.Alternative.FreeStar

import Options.Applicative.Name
import Options.Applicative.Validate

class HasOption f where
  mkOption :: Names -> Validate a -> f a

class HasFlag f where
  mkFlag :: Names -> a -> f a

class HasCommand f where
  mkCommand :: String -> a -> f a

class HasArgument f where
  mkArgument :: String -> Validate a -> f a

instance HasOption f => HasOption (Alt f) where
  mkOption n v = liftAlt $ mkOption n v

instance HasFlag f => HasFlag (Alt f) where
  mkFlag n x = liftAlt $ mkFlag n x

instance HasCommand f => HasCommand (Alt f) where
  mkCommand n x = liftAlt $ mkCommand n x
