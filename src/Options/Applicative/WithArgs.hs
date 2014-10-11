{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Options.Applicative.WithArgs where

import Control.Alternative.FreeStar
import Control.Applicative
import Data.Functor.Compose
import Data.Functor.Identity

import Options.Applicative.Basic
import Options.Applicative.Classes
import Options.Applicative.Types

newtype WithArgs f a = WithArgs { unWithArgs :: OptSum f (WithDesc OptProperties Identity) a }
  deriving (Functor, Opt)

instance HasOption f => HasOption (WithArgs f) where
  mkOption prop n v = WithArgs . OptLeft $ mkOption prop n v

instance HasFlag f => HasFlag (WithArgs f) where
  mkFlag prop n x = WithArgs . OptLeft $ mkFlag prop n x

instance HasCommand f => HasCommand (WithArgs f) where
  mkCommand n x = WithArgs . OptLeft $ mkCommand n x

newtype Parser a = Parser
  { unParser :: Compose (Alt (WithArgs (WithSub ParserInfo BaseOption))) ArgParser a }
  deriving (Functor, Applicative, Alternative, OptParser)

instance HasArgument Parser where
  mkArgument prop v = Parser . Compose . liftAlt
                    . WithArgs . OptRight . WithDesc prop
                    . Identity . argParser1 $ v

instance HasOption Parser where
  mkOption prop n v = Parser . Compose . fmap pure . mkOption prop n $ v

instance HasFlag Parser where
  mkFlag prop n x = Parser . Compose . fmap pure . mkFlag prop n $ x

instance HasCommand Parser where
  mkCommand n x = Parser . Compose . fmap pure . mkCommand n $ x

-- | A full description for a runnable 'Parser' for a program.
type ParserInfo = WithInfo Metadata Parser

overInfo :: (i -> i) -> WithInfo i f a -> WithInfo i f a
overInfo f i = i { bundledInfo = f (bundledInfo i) }
