module Options.Applicative.Classes where

import Control.Alternative.FreeStar
import Control.Applicative

import Options.Applicative.Basic
import Options.Applicative.Types

class HasOption f where
  mkOption :: OptProperties -> [OptName] -> ReadM a -> f a

class HasFlag f where
  mkFlag :: OptProperties -> [OptName] -> a -> f a

class HasCommand f where
  mkCommand :: String -> a -> f a

class HasArgument f where
  mkArgument :: OptProperties -> ReadM a -> f a

class HasMetadata f where
  getMetadata :: f a -> Metadata

instance HasOption f => HasOption (Alt f) where
  mkOption prop n v = liftAlt $ mkOption prop n v

instance HasFlag f => HasFlag (Alt f) where
  mkFlag prop n x = liftAlt $ mkFlag prop n x

instance HasCommand f => HasCommand (Alt f) where
  mkCommand n x = liftAlt $ mkCommand n x

instance (Applicative f, HasOption f) => HasOption (WithSub f) where
  mkOption prop n v = liftSub $ mkOption prop n v

instance (Applicative f, HasFlag f) => HasFlag (WithSub f) where
  mkFlag prop n x = liftSub $ mkFlag prop n x

instance (Applicative f, HasCommand f) => HasCommand (WithSub f) where
  mkCommand n x = liftSub $ mkCommand n x

instance (Applicative f, HasArgument f) => HasArgument (WithSub f) where
  mkArgument prop v = liftSub $ mkArgument prop v
