{-# LANGUAGE FlexibleContexts #-}
module Options.Applicative.Error where

import Control.Monad.Except
import Data.Semigroup

newtype ParseError = ParseError
  { unParseError :: Option (Last ParseError1) }
  deriving (Eq, Ord, Read, Show)

instance Monoid ParseError where
  mempty = ParseError mempty
  mappend x y = ParseError $ mappend (unParseError x) (unParseError y)

data ParseError1 = ErrMsg String
  deriving (Eq, Ord, Read, Show)

type Err = Except ParseError

errMsg :: MonadError ParseError m => String -> m a
errMsg = throwError . ParseError . Option . Just . Last . ErrMsg
