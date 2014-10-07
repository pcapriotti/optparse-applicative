{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Options.Applicative.WithArgs where

import Control.Alternative.FreeStar
import Control.Applicative
import Data.Functor.Compose
import Data.Functor.Identity

import Options.Applicative.Basic
import Options.Applicative.Classes
import Options.Applicative.Types

newtype WithArgs f a = WithArgs { unWithArgs :: OptSum f Identity a }
  deriving (Functor, Opt)

instance HasOption f => HasOption (WithArgs f) where
  mkOption prop n v = WithArgs . OptLeft $ mkOption prop n v

instance HasFlag f => HasFlag (WithArgs f) where
  mkFlag prop n x = WithArgs . OptLeft $ mkFlag prop n x

instance HasCommand f => HasCommand (WithArgs f) where
  mkCommand n x = WithArgs . OptLeft $ mkCommand n x

instance HasArgument (Compose (Alt (WithDesc OptProperties (WithArgs f))) ArgParser) where
  mkArgument prop v
    = Compose . liftAlt . WithDesc prop
    . WithArgs . OptRight . Identity
    $ argParser1 v

instance (Functor f, HasOption f, Applicative g) => HasOption (Compose f g) where
  mkOption prop n v = Compose . fmap pure . mkOption prop n $ v

instance (Functor f, HasFlag f, Applicative g) => HasFlag (Compose f g) where
  mkFlag prop n x = Compose . fmap pure . mkFlag prop n $ x

instance (Functor f, HasCommand f, Applicative g) => HasCommand (Compose f g) where
  mkCommand n x = Compose . fmap pure . mkCommand n $ x

type Parser = Compose (Alt (WithDesc OptProperties (WithArgs BaseOption))) ArgParser

-- | A full description for a runnable 'Parser' for a program.
type ParserInfo = WithInfo Metadata Parser

overMetadata :: (Metadata -> Metadata) -> ParserInfo a -> ParserInfo a
overMetadata f i = i { bundledInfo = f (bundledInfo i) }
