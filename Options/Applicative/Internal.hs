{-# LANGUAGE GADTs, RankNTypes #-}
module Options.Applicative.Internal
  ( P
  , Context(..)
  , MonadP(..)
  , ParseError(..)

  , uncons
  , hoistMaybe
  , hoistEither

  , runP

  , Completion
  , runCompletion
  , SomeParser(..)
  , ComplError(..)

  , ListT
  , takeListT
  , runListT
  ) where

import Control.Applicative (Applicative(..), Alternative(..), (<$>))
import Control.Monad (MonadPlus(..), liftM, ap)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Error
  (runErrorT, ErrorT(..), Error(..), throwError, catchError)
import Control.Monad.Trans.Reader
  (runReader, runReaderT, Reader, ReaderT, ask)
import Control.Monad.Trans.Writer (runWriterT, WriterT, tell)
import Data.Maybe (maybeToList)
import Data.Monoid (Monoid(..))

import Options.Applicative.Types

class (Alternative m, MonadPlus m) => MonadP m where
  setContext :: Maybe String -> ParserInfo a -> m ()
  setParser :: Maybe String -> Parser a -> m ()
  getPrefs :: m ParserPrefs

  missingArgP :: ParseError -> Completer -> m a
  tryP :: m a -> m (Either ParseError a)
  errorP :: ParseError -> m a
  exitP :: Parser b -> Maybe a -> m a

newtype P a = P (ErrorT ParseError (WriterT Context (Reader ParserPrefs)) a)

instance Functor P where
  fmap f (P m) = P $ fmap f m

instance Applicative P where
  pure a = P $ pure a
  P f <*> P a = P $ f <*> a

instance Alternative P where
  empty = P empty
  P x <|> P y = P $ x <|> y

instance Monad P where
  return a = P $ return a
  P x >>= k = P $ x >>= \a -> case k a of P y -> y

instance MonadPlus P where
  mzero = P mzero
  mplus (P x) (P y) = P $ mplus x y


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
  setContext name = P . lift . tell . Context (maybeToList name)
  setParser _ _ = return ()
  getPrefs = P . lift . lift $ ask

  missingArgP e _ = errorP e
  tryP (P p) = P $ lift $ runErrorT p
  exitP _ = P . hoistMaybe
  errorP = P . throwError

hoistMaybe :: MonadPlus m => Maybe a -> m a
hoistMaybe = maybe mzero return

hoistEither :: MonadP m => Either ParseError a -> m a
hoistEither = either errorP return

runP :: P a -> ParserPrefs -> (Either ParseError a, Context)
runP (P p) = runReader . runWriterT . runErrorT $ p

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

newtype Completion a =
  Completion (ErrorT ParseError (ReaderT ParserPrefs ComplResult) a)

instance Functor Completion where
  fmap f (Completion m) = Completion $ fmap f m

instance Applicative Completion where
  pure a = Completion $ pure a
  Completion f <*> Completion a = Completion $ f <*> a

instance Alternative Completion where
  empty = Completion empty
  Completion x <|> Completion y = Completion $ x <|> y

instance Monad Completion where
  return a = Completion $ return a
  Completion x >>= k = Completion $ x >>= \a -> case k a of Completion y -> y

instance MonadPlus Completion where
  mzero = Completion mzero
  mplus (Completion x) (Completion y) = Completion $ mplus x y

instance MonadP Completion where
  setContext _ _ = return ()
  setParser _ _ = return ()
  getPrefs = Completion $ lift ask

  missingArgP _ = Completion . lift . lift . ComplOption
  tryP (Completion p) = Completion $ catchError (Right <$> p) (return . Left)
  exitP p _ = Completion . lift . lift . ComplParser $ SomeParser p
  errorP = Completion . throwError

runCompletion :: Completion r -> ParserPrefs -> Maybe (Either SomeParser Completer)
runCompletion (Completion c) prefs = case runReaderT (runErrorT c) prefs of
  ComplResult _ -> Nothing
  ComplParser p' -> Just $ Left p'
  ComplOption compl -> Just $ Right compl

-- A "ListT done right" implementation

newtype ListT m a = ListT
  { stepListT :: m (TStep a (ListT m a)) }

data TStep a x
  = TNil
  | TCons a x

bimapTStep :: (a -> b) -> (x -> y) -> TStep a x -> TStep b y
bimapTStep _ _ TNil = TNil
bimapTStep f g (TCons a x) = TCons (f a) (g x)

hoistList :: Monad m => [a] -> ListT m a
hoistList = foldr (\x xt -> ListT (return (TCons x xt))) mzero

takeListT :: Monad m => Int -> ListT m a -> ListT m a
takeListT 0 = const mzero
takeListT n = ListT . liftM (bimapTStep id (takeListT (n - 1))) . stepListT

runListT :: Monad m => ListT m a -> m [a]
runListT xs = do
  s <- stepListT xs
  case s of
    TNil -> return []
    TCons x xt -> liftM (x :) (runListT xt)

instance Monad m => Functor (ListT m) where
  fmap f = ListT
         . liftM (bimapTStep f (fmap f))
         . stepListT

instance Monad m => Applicative (ListT m) where
  pure = hoistList . pure
  (<*>) = ap

instance Monad m => Monad (ListT m) where
  return = pure
  xs >>= f = ListT $ do
    s <- stepListT xs
    case s of
      TNil -> return TNil
      TCons x xt -> stepListT $ f x `mplus` (xt >>= f)

instance Monad m => Alternative (ListT m) where
  empty = mzero
  (<|>) = mplus

instance MonadTrans ListT where
  lift = ListT . liftM (`TCons` mzero)

instance Monad m => MonadPlus (ListT m) where
  mzero = ListT (return TNil)
  mplus xs ys = ListT $ do
    s <- stepListT xs
    case s of
      TNil -> stepListT ys
      TCons x xt -> return $ TCons x (xt `mplus` ys)
