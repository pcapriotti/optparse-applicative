{-# LANGUAGE GADTs, FlexibleInstances, TypeFamilies #-}
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
  , ComplError(..)
  , exitCompletion
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Error
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Data.Maybe
import Data.Monoid

import Options.Applicative.Types

class (Alternative m, MonadPlus m) => MonadP m where
  type PError m

  nextArg :: Maybe String -> m (Maybe String)
  setContext :: Maybe String -> ParserInfo a -> m ()
  setParser :: Maybe String -> Parser a -> m ()

  tryP :: m a -> m (Either (PError m) a)
  errorP :: PError m -> m a

type P = StateT [String] (ErrorT String (Writer Context))

data Context where
  Context :: Maybe String -> ParserInfo a -> Context
  NullContext :: Context

instance Monoid Context where
  mempty = NullContext
  mappend _ c@(Context _ _) = c
  mappend c _ = c

instance MonadP P where
  type PError P = String

  nextArg val = do
    args <- getArgs
    case maybeToList val ++ args of
      [] -> return Nothing
      (arg':args') -> do
        setArgs args'
        return $ Just arg'

  setContext name = lift . lift . tell . Context name
  setParser _ _ = return ()

  errorP = lift . throwError

  tryP p = do
    args <- getArgs
    let (r, ctx) = runP p args
    lift . lift . tell $ ctx
    case r of
      Left e -> return (Left e)
      Right (x, args') -> do
        setArgs args'
        return (Right x)

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

data SomeParser where
  SomeParser :: Parser a -> SomeParser

data ComplState = ComplState
  { complWords :: [String]
  , complIndex :: !Int
  , complParser :: SomeParser
  , complArg :: String }

data ComplError
  = ComplParseError String
  | ComplExit

instance Error ComplError where
  strMsg = ComplParseError

type Completion = ErrorT ComplError (State ComplState)

instance MonadP Completion where
  type PError Completion = ComplError

  nextArg val = do
    st <- lift get
    let i = complIndex st
    unless (i > 0) exitCompletion
    case val of
      Just arg -> return $ Just arg
      Nothing -> do
        let ws = complWords st
        (arg, ws') <- liftMaybe (uncons ws)
        lift . modify $ \s -> s
          { complWords = ws'
          , complIndex = i - 1 }
        return $ Just arg

  setContext val i = setParser val (infoParser i)
  setParser val p = lift . modify $ \s -> s
    { complParser = SomeParser p
    , complArg = fromMaybe "" val }

  errorP = throwError

  tryP p = do
    r <- lift $ runErrorT p
    case r of
      Left e@(ComplParseError _) -> return (Left e)
      Left e -> throwError e
      Right x -> return (Right x)

runCompletion :: Completion r -> [String] -> Int -> Parser a -> Either ComplError r
runCompletion c ws i p = evalState (runErrorT c) s
  where s = ComplState ws i (SomeParser p) ""

exitCompletion :: Completion ()
exitCompletion = throwError ComplExit
