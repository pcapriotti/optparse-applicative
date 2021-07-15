{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Options.Applicative.Internal
  ( P
  , MonadP(..)
  , ParseError(..)

  , uncons
  , hoistMaybe
  , hoistEither
  , runReadM
  , withReadM

  , runP

  , Completion
  , runCompletion
  , contextNames

  , ListT
  , takeListT
  , runListT

  , NondetT
  , cut
  , (<!>)
  , disamb
  ) where

import Control.Applicative
import Prelude
import Control.Monad (MonadPlus(..), liftM, ap, guard)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Except
  (runExcept, runExceptT, withExcept, ExceptT(..), throwE)
import Control.Monad.Trans.Reader
  (mapReaderT, runReader, runReaderT, Reader, ReaderT, ask)
import Control.Monad.Trans.State (StateT, get, put, modify, evalStateT, runStateT)
import Data.Proxy

import Options.Applicative.Types

class (Alternative m, MonadPlus m) => MonadP ann m where
  enterContext :: String -> ParserInfo ann a -> m ()
  exitContext :: Proxy ann -> m ()
  getPrefs :: Proxy ann -> m ParserPrefs

  missingArgP :: Proxy ann -> ParseError ann -> Completer -> m a
  errorP :: Proxy ann -> ParseError ann -> m a
  exitP :: IsCmdStart -> ArgPolicy -> Parser ann b -> Maybe a -> m a

data P ann a = P (ExceptT (ParseError ann) (StateT [Context ann] (Reader ParserPrefs)) a)

instance Functor (P ann) where
  fmap f (P m) = P $ fmap f m

instance Applicative (P ann) where
  pure a = P $ pure a
  P f <*> P a = P $ f <*> a

instance Alternative (P ann) where
  empty = P empty
  P x <|> P y = P $ x <|> y

instance Monad (P ann) where
  return = pure
  P x >>= k = P $ x >>= \a -> case k a of P y -> y

instance MonadPlus (P ann) where
  mzero = P mzero
  mplus (P x) (P y) = P $ mplus x y

contextNames :: [Context ann] -> [String]
contextNames ns =
  let go (Context n _) = n
  in  reverse $ go <$> ns

instance MonadP ann (P ann) where
  enterContext name pinfo = P $ lift $ modify $ (:) $ Context name pinfo
  exitContext _ = P $ lift $ modify $ drop 1
  getPrefs _ = P . lift . lift $ ask

  missingArgP proxy e _ = errorP proxy e
  exitP i _ p = P . maybe (throwE . MissingError i . SomeParser $ p) return
  errorP _ = P . throwE

hoistMaybe :: MonadPlus m => Maybe a -> m a
hoistMaybe = maybe mzero return

hoistEither :: MonadP ann m => Proxy ann -> Either (ParseError ann) a -> m a
hoistEither proxy = either (errorP proxy) return

runP :: P ann a -> ParserPrefs -> (Either (ParseError ann) a, [Context ann])
runP (P p) = runReader . flip runStateT [] . runExceptT $ p

uncons :: [a] -> Maybe (a, [a])
uncons [] = Nothing
uncons (x : xs) = Just (x, xs)

runReadM :: MonadP ann m => Proxy ann -> ReadM ann a -> String -> m a
runReadM proxy (ReadM r) s = hoistEither proxy . runExcept $ runReaderT r s

withReadM :: (String -> String) -> ReadM ann a -> ReadM ann a
withReadM f = ReadM . mapReaderT (withExcept f') . unReadM
  where
    f' (ErrorMsg err) = ErrorMsg (f err)
    f' e = e

data ComplResult ann a
  = ComplParser (SomeParser ann) ArgPolicy
  | ComplOption Completer
  | ComplResult a

instance Functor (ComplResult ann) where
  fmap = liftM

instance Applicative (ComplResult ann) where
  pure = ComplResult
  (<*>) = ap

instance Monad (ComplResult ann) where
  return = pure
  m >>= f = case m of
    ComplResult r -> f r
    ComplParser p a -> ComplParser p a
    ComplOption c -> ComplOption c

newtype Completion ann a =
  Completion (ExceptT (ParseError ann) (ReaderT ParserPrefs (ComplResult ann)) a)

instance Functor (Completion ann) where
  fmap f (Completion m) = Completion $ fmap f m

instance Applicative (Completion ann) where
  pure a = Completion $ pure a
  Completion f <*> Completion a = Completion $ f <*> a

instance Alternative (Completion ann) where
  empty = Completion empty
  Completion x <|> Completion y = Completion $ x <|> y

instance Monad (Completion ann) where
  return = pure
  Completion x >>= k = Completion $ x >>= \a -> case k a of Completion y -> y

instance MonadPlus (Completion ann) where
  mzero = Completion mzero
  mplus (Completion x) (Completion y) = Completion $ mplus x y

instance MonadP ann (Completion ann) where
  enterContext _ _ = return ()
  exitContext _ = return ()
  getPrefs _ = Completion $ lift ask

  missingArgP _ _ = Completion . lift . lift . ComplOption
  exitP _ a p _ = Completion . lift . lift $ ComplParser (SomeParser p) a
  errorP _ = Completion . throwE

runCompletion :: Completion ann r -> ParserPrefs -> Maybe (Either (SomeParser ann, ArgPolicy) Completer)
runCompletion (Completion c) prefs = case runReaderT (runExceptT c) prefs of
  ComplResult _ -> Nothing
  ComplParser p' a' -> Just $ Left (p', a')
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

-- nondeterminism monad with cut operator

newtype NondetT m a = NondetT
  { runNondetT :: ListT (StateT Bool m) a }

instance Monad m => Functor (NondetT m) where
  fmap f = NondetT . fmap f . runNondetT

instance Monad m => Applicative (NondetT m) where
  pure = NondetT . pure
  NondetT m1 <*> NondetT m2 = NondetT (m1 <*> m2)

instance Monad m => Monad (NondetT m) where
  return = pure
  NondetT m1 >>= f = NondetT $ m1 >>= runNondetT . f

instance Monad m => MonadPlus (NondetT m) where
  mzero = NondetT mzero
  NondetT m1 `mplus` NondetT m2 = NondetT (m1 `mplus` m2)

instance Monad m => Alternative (NondetT m) where
  empty = mzero
  (<|>) = mplus

instance MonadTrans NondetT where
  lift = NondetT . lift . lift

(<!>) :: Monad m => NondetT m a -> NondetT m a -> NondetT m a
(<!>) m1 m2 = NondetT . mplus (runNondetT m1) $ do
  s <- lift get
  guard (not s)
  runNondetT m2

cut :: Monad m => NondetT m ()
cut = NondetT $ lift (put True)

disamb :: Monad m => Bool -> NondetT m a -> m (Maybe a)
disamb allow_amb xs = do
  xs' <- (`evalStateT` False)
       . runListT
       . takeListT (if allow_amb then 1 else 2)
       . runNondetT $ xs
  return $ case xs' of
    [x] -> Just x
    _   -> Nothing
