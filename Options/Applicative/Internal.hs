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

  missingArgP :: Completer -> m a
  tryP :: m a -> m (Either (PError m) a)
  errorP :: PError m -> m a
  exitP :: Parser b -> m a

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

  missingArgP _ = empty
  tryP p = lift $ runErrorT p
  exitP _ = mzero
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

data ComplResult a
  = ComplParser SomeParser
  | ComplOption Completer
  | ComplResult a

instance Functor ComplResult where
  fmap = liftM

instance Applicative ComplResult where
  pure = ComplResult
  (<*>) = ap

instance Monad ComplResult where
  return = pure
  m >>= f = case m of
    ComplResult r -> f r
    ComplParser p -> ComplParser p
    ComplOption c -> ComplOption c

type Completion = ErrorT String (StateT ComplState ComplResult)

instance MonadP Completion where
  type PError Completion = String

  setContext val i = setParser val (infoParser i)
  setParser val p = lift . modify $ \s -> s
    { complParser = SomeParser p
    , complArg = fromMaybe "" val }

  missingArgP = lift . lift . ComplOption
  tryP p = catchError (Right <$> p) (return . Left)
  exitP = lift . lift . ComplParser . SomeParser
  errorP = throwError

runCompletion :: Completion r -> Parser a -> Maybe (Either SomeParser Completer)
runCompletion c p = case runStateT (runErrorT c) s of
  ComplResult _ -> Nothing
  ComplParser p' -> Just $ Left p'
  ComplOption compl -> Just $ Right compl
  where s = ComplState (SomeParser p) ""
