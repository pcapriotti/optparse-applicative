{-# LANGUAGE GADTs, FlexibleInstances #-}
module Options.Applicative.Internal
  ( P
  , Context(..)
  , MonadP(..)

  , uncons
  , liftMaybe
  , getArgs
  , setArgs

  , runP
  , runCompletion
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Error
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Data.Maybe
import Data.Monoid

import Options.Applicative.Types

class (Alternative m, MonadPlus m) => MonadP m where
  nextArg :: Maybe String -> m String
  setContext :: Maybe String -> ParserInfo a -> m ()
  tryP :: m a -> m a -> m a

type P = StateT [String] (ErrorT String (Writer Context))

data Context where
  Context :: Maybe String -> ParserInfo a -> Context
  NullContext :: Context

instance Monoid Context where
  mempty = NullContext
  mappend _ c@(Context _ _) = c
  mappend c _ = c

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

data ComplState = ComplState [String] !Int
type Completion = MaybeT (State ComplState)

instance MonadP Completion where
  nextArg val = do
    ComplState ws i <- lift get
    guard $ i > 0
    case val of
      Just arg -> return arg
      Nothing -> do
        (arg, ws') <- liftMaybe (uncons ws)
        lift . put $ ComplState ws' (i - 1)
        return arg
  setContext _ _ = return ()

  tryP result p = do
    r <- lift $ runMaybeT p
    case r of
      Nothing -> result
      Just x -> return x

runCompletion :: Completion a -> [String] -> Int -> Maybe a
runCompletion c ws i = evalState (runMaybeT c) s
  where s = ComplState ws i
