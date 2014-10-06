{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Options.Applicative.WithArgs where

import Control.Alternative.FreeStar
import Data.Functor.Compose
import Data.Functor.Identity

import Options.Applicative.Basic
import Options.Applicative.Classes

newtype WithArgs f a = WithArgs
  { unWithArgs :: OptSum f (WithDesc String Identity) a }
  deriving (Functor, Opt, Pretty1)

instance HasOption f => HasOption (WithArgs f) where
  mkOption n v = WithArgs . OptLeft $ mkOption n v

instance HasFlag f => HasFlag (WithArgs f) where
  mkFlag n x = WithArgs . OptLeft $ mkFlag n x

instance HasCommand f => HasCommand (WithArgs f) where
  mkCommand n x = WithArgs . OptLeft $ mkCommand n x

instance HasArgument (Compose (Alt (WithArgs f)) ArgParser) where
  mkArgument d v = Compose . liftAlt . WithArgs
                 . OptRight . WithDesc d . Identity
                 $ argParser1 v
