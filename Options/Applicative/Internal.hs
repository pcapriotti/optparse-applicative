{-# LANGUAGE GADTs, FlexibleInstances, TypeFamilies #-}
module Options.Applicative.Internal
  ( P
  , Context(..)
  , MonadP(..)

  , uncons
  , liftMaybe

  , runP

  , Completion
  , runCompletion
  , SomeParser(..)
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

  setContext :: Maybe String -> ParserInfo a -> m ()
  setParser :: Maybe String -> Parser a -> m ()

  tryP :: m a -> m (Either (PError m) a)
  errorP :: PError m -> m a
  exitP :: m a

type P = ErrorT String (Writer Context)

data Context where
  Context :: Maybe String -> ParserInfo a -> Context
  NullContext :: Context

instance Monoid Context where
  mempty = NullContext
  mappend _ c@(Context _ _) = c
  mappend c _ = c

instance MonadP P where
  type PError P = String

  setContext name = lift . tell . Context name
  setParser _ _ = return ()


  tryP p = lift $ runErrorT p
  exitP = mzero
  errorP = throwError

liftMaybe :: MonadPlus m => Maybe a -> m a
liftMaybe = maybe mzero return

runP :: P a -> (Either String a, Context)
runP = runWriter . runErrorT

uncons :: [a] -> Maybe (a, [a])
uncons [] = Nothing
uncons (x : xs) = Just (x, xs)

data SomeParser where
  SomeParser :: Parser a -> SomeParser

data ComplState = ComplState
  { complParser :: SomeParser
  , complArg :: String }

data ComplError
  = ComplParseError String
  | ComplExit

instance Error ComplError where
  strMsg = ComplParseError

type Completion = ErrorT ComplError (State ComplState)

instance MonadP Completion where
  type PError Completion = ComplError

  setContext val i = setParser val (infoParser i)
  setParser val p = lift . modify $ \s -> s
    { complParser = SomeParser p
    , complArg = fromMaybe "" val }

  tryP p = do
    r <- lift $ runErrorT p
    case r of
      Left e@(ComplParseError _) -> return (Left e)
      Left e -> throwError e
      Right x -> return (Right x)
  exitP = throwError ComplExit
  errorP = throwError

runCompletion :: Completion r -> Parser a -> (Either ComplError r, SomeParser, String)
runCompletion c p = case runState (runErrorT c) s of
  (r, s') -> (r, complParser s', complArg s')
  where s = ComplState (SomeParser p) ""

exitCompletion :: Completion ()
exitCompletion = throwError ComplExit
