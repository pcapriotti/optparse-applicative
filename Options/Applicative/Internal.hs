{-# LANGUAGE GADTs, FlexibleInstances #-}
module Options.Applicative.Internal
  ( P
  , Context(..)
  , MonadP(..)

  , liftMaybe
  , runP
  , getArgs
  , setArgs

  , uncons
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Error
import Control.Monad.Trans.State
import Data.Maybe
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

class (Alternative m, MonadPlus m) => MonadP m where
  nextArg :: Maybe String -> m String
  setContext :: Maybe String -> ParserInfo a -> m ()
  tryP :: m a -> m a -> m a

instance MonadP P where
  nextArg val = do
    args <- getArgs
    (arg', args') <- liftMaybe . uncons $ maybeToList val ++ args
    setArgs args'
    return arg'

  setContext name = lift . lift . tell . Context name

  tryP result p = do
    args <- getArgs
    let (r, ctx) = runP p args
    lift . lift . tell $ ctx
    case r of
      Left e -> result `mplus` lift (throwError e)
      Right (x, args') -> do
        setArgs args'
        return x

liftMaybe :: MonadPlus m => Maybe a -> m a
liftMaybe = maybe mzero return

runP :: P a -> [String] -> (Either String (a, [String]), Context)
runP p args = runWriter . runErrorT $ runStateT p args

getArgs :: P [String]
getArgs = get

setArgs :: [String] -> P ()
setArgs = put

uncons :: [a] -> Maybe (a, [a])
uncons [] = Nothing
uncons (x : xs) = Just (x, xs)
