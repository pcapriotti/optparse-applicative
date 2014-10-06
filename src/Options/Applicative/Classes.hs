module Options.Applicative.Classes where

import Control.Alternative.FreeStar

import Options.Applicative.Name
import Options.Applicative.Types

class HasOption f where
  mkOption :: OptProperties -> Names -> ReadM a -> f a

class HasFlag f where
  mkFlag :: OptProperties -> Names -> a -> f a

class HasCommand f where
  mkCommand :: OptProperties -> String -> a -> f a

class HasArgument f where
  mkArgument :: OptProperties -> ReadM a -> f a

instance HasOption f => HasOption (Alt f) where
  mkOption prop n v = liftAlt $ mkOption prop n v

instance HasFlag f => HasFlag (Alt f) where
  mkFlag prop n x = liftAlt $ mkFlag prop n x

instance HasCommand f => HasCommand (Alt f) where
  mkCommand prop n x = liftAlt $ mkCommand prop n x
