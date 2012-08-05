{-# LANGUAGE GADTs, FlexibleInstances #-}
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
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Data.Maybe
import Data.Monoid

import Options.Applicative.Types

class (Alternative m, MonadPlus m) => MonadP m where
  setContext :: Maybe String -> ParserInfo a -> m ()
  setParser :: Maybe String -> Parser a -> m ()
  getPrefs :: m ParserPrefs

  missingArgP :: Completer -> m a
  tryP :: m a -> m (Either String a)
  errorP :: String -> m a
  exitP :: Parser b -> m a

type P = ErrorT String (WriterT Context (Reader ParserPrefs))

data Context where
  Context :: [String] -> ParserInfo a -> Context
  NullContext :: Context

contextNames :: Context -> [String]
contextNames (Context ns _) = ns
contextNames NullContext = []

instance Monoid Context where
  mempty = NullContext
  mappend c (Context ns i) = Context (contextNames c ++ ns) i
  mappend c _ = c

instance MonadP P where
  setContext name = lift . tell . Context (maybeToList name)
  setParser _ _ = return ()
  getPrefs = lift . lift $ ask

  missingArgP _ = empty
  tryP p = lift $ runErrorT p
  exitP _ = mzero
  errorP = throwError

liftMaybe :: MonadPlus m => Maybe a -> m a
liftMaybe = maybe mzero return

runP :: P a -> ParserPrefs -> (Either String a, Context)
runP = runReader . runWriterT . runErrorT

uncons :: [a] -> Maybe (a, [a])
uncons [] = Nothing
uncons (x : xs) = Just (x, xs)

data SomeParser where
  SomeParser :: Parser a -> SomeParser

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

type Completion = ErrorT String (ReaderT ParserPrefs ComplResult)

instance MonadP Completion where
  setContext _ _ = return ()
  setParser _ _ = return ()
  getPrefs = lift ask

  missingArgP = lift . lift . ComplOption
  tryP p = catchError (Right <$> p) (return . Left)
  exitP = lift . lift . ComplParser . SomeParser
  errorP = throwError

runCompletion :: Completion r -> ParserPrefs -> Maybe (Either SomeParser Completer)
runCompletion c prefs = case runReaderT (runErrorT c) prefs of
  ComplResult _ -> Nothing
  ComplParser p' -> Just $ Left p'
  ComplOption compl -> Just $ Right compl
