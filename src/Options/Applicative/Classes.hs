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

instance (Functor f, HasOption f) => HasOption (WithSub p f) where
  mkOption prop n v = liftSub $ mkOption prop n v

instance (Functor f, HasFlag f) => HasFlag (WithSub p f) where
  mkFlag prop n x = liftSub $ mkFlag prop n x

instance (Functor f, HasCommand f) => HasCommand (WithSub p f) where
  mkCommand n x = liftSub $ mkCommand n x

instance (Functor f, HasArgument f) => HasArgument (WithSub p f) where
  mkArgument prop v = liftSub $ mkArgument prop v

instance HasOption BaseOption where
  mkOption = RegOption

instance HasFlag BaseOption where
  mkFlag = Flag

instance HasCommand BaseOption where
  mkCommand = Command
