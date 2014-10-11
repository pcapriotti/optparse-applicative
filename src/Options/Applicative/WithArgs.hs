{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances #-}
module Options.Applicative.WithArgs where

import Control.Alternative.FreeStar
import Control.Applicative
import Data.Functor.Compose
import Data.Functor.Identity

import Options.Applicative.Basic
import Options.Applicative.Classes
import Options.Applicative.Types

type Option = WithInfo OptProperties (OptSum (WithSub ParserInfo BaseOption) Identity)

newtype Parser a = Parser
  { unParser :: Compose (Alt Option) ArgParser a }
  deriving ( Functor, Applicative, Alternative, OptParser, HasUsage
           , HasOption (WithInfo OptProperties BaseOption)
           , HasSubOption ParserInfo (WithInfo OptProperties BaseOption) )

instance HasOption (WithInfo OptProperties Argument) Parser where
  liftOption (WithInfo i arg) = Parser . Compose . liftAlt
                              . WithInfo i . OptRight . Identity
                              . argParser1 . unArgument $ arg

-- | A full description for a runnable 'Parser' for a program.
type ParserInfo = WithInfo Metadata Parser
