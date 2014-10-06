{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Options.Applicative.Validate where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Except

import Options.Applicative.Error

newtype Validate a = Validate
  { runValidate :: ReaderT String Err a }
  deriving ( Functor, Applicative, Monad, Alternative, MonadPlus
           , MonadReader String, MonadError ParseError )
