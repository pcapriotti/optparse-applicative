{-# LANGUAGE GADTs #-}
module Options.Applicative.Internal
  ( P
  , Context(..)
  ) where

import Control.Monad.Trans.Writer
import Control.Monad.Trans.Error
import Control.Monad.Trans.State
import Data.Monoid

import Options.Applicative.Types

type P = StateT [String] (ErrorT String (Writer Context))

data Context where
  Context :: Maybe String -> ParserInfo a -> Context
  NullContext :: Context

instance Monoid Context where
  mempty = NullContext
  mappend _ c@(Context _ _) = c
  mappend c _ = c
