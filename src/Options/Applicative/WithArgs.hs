{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Options.Applicative.WithArgs where

import Control.Alternative.FreeStar
import Data.Functor.Compose
import Data.Functor.Identity

import Options.Applicative.Basic
import Options.Applicative.Classes
import Options.Applicative.Types

newtype WithArgs f a = WithArgs
  { unWithArgs :: OptSum f (WithDesc OptProperties Identity) a }
  deriving (Functor, Opt)

instance HasOption f => HasOption (WithArgs f) where
  mkOption prop n v = WithArgs . OptLeft $ mkOption prop n v

instance HasFlag f => HasFlag (WithArgs f) where
  mkFlag prop n x = WithArgs . OptLeft $ mkFlag prop n x

instance HasCommand f => HasCommand (WithArgs f) where
  mkCommand prop n x = WithArgs . OptLeft $ mkCommand prop n x

instance HasArgument (Compose (Alt (WithArgs f)) ArgParser) where
  mkArgument prop v
    = Compose . liftAlt . WithArgs
    . OptRight . WithDesc prop . Identity
    $ argParser1 v
