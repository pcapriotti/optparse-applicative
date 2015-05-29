{-# LANGUAGE RankNTypes #-}
-- | Free non-distributive 'Alternative' functor.
module Control.Alternative.FreeND
  ( Alt(..)
  , Alt1(..)
  , Alt2(..)
  , liftAlt
  , runAlt
  , runAlt1
  , runAlt2
  , hoistAlt
  , altToAp
  , apToAlt
  , altToList1
  , list1ToAlt
  ) where

import Control.Applicative
import Control.Applicative.Free
import Control.Applicative.Free.Complement
import Data.Functor.Coproduct
import Data.Monoid1

newtype Alt1 f a = Alt1
  { unAlt1 :: Ap' (Coproduct f (Alt2 f)) a }
newtype Alt2 f a = Alt2
  { unAlt2 :: List1' (Coproduct f (Alt1 f)) a }

instance Functor (Alt1 f) where
  fmap f = Alt1 . fmap f . unAlt1

instance Applicative (Alt1 f) where
  pure = Alt1 . pure
  f <*> x = Alt1 $ unAlt1 f <*> unAlt1 x

instance Functor f => Functor (Alt2 f) where
  fmap f = Alt2 . fmap f . unAlt2

instance Functor f => Monoid1 (Alt2 f) where
  mempty1 = Alt2 mempty1
  mappend1 x y = Alt2 $ mappend1 (unAlt2 x) (unAlt2 y)

-- | Coproduct of consistent monads.
--
-- See <http://arxiv.org/abs/1409.3804 Coproducts of Monads on Set>
-- for more details on this construction.
data Alt f a
  = LiftAlt (f a)
  | Branch1 (Alt1 f a)
  | Branch2 (Alt2 f a)

runAlt :: (Functor f, Alternative g) => (forall x . f x -> g x) -> Alt f a -> g a
runAlt f (LiftAlt x) = f x
runAlt f (Branch1 b1) = runAlt1 f b1
runAlt f (Branch2 b2) = runAlt2 f b2

runAlt1 :: (Functor f, Alternative g) => (forall x . f x -> g x) -> Alt1 f a -> g a
runAlt1 f (Alt1 x) = runAp (coproduct f (runAlt2 f)) (ap'ToAp x)

runAlt2 :: (Functor f, Alternative g) => (forall x . f x -> g x) -> Alt2 f a -> g a
runAlt2 f (Alt2 x) = runList1Alt (coproduct f (runAlt1 f)) (list1'ToList1 x)

liftAlt :: f a -> Alt f a
liftAlt = LiftAlt

hoistAlt :: (forall x. f x -> g x) -> Alt f a -> Alt g a
hoistAlt phi (LiftAlt x) = LiftAlt (phi x)
hoistAlt phi (Branch1 b1) = Branch1 $ hoistAlt1 phi b1
hoistAlt phi (Branch2 b2) = Branch2 $ hoistAlt2 phi b2

hoistAlt1 :: (forall x. f x -> g x) -> Alt1 f a -> Alt1 g a
hoistAlt1 phi (Alt1 x) = Alt1 $ hoistAp'  (Coproduct . coproduct
                                            (Left . phi)
                                            (Right . hoistAlt2 phi))
                                          x

hoistAlt2 :: (forall x. f x -> g x) -> Alt2 f a -> Alt2 g a
hoistAlt2 phi (Alt2 x) = Alt2 $ hoistList1' (Coproduct . coproduct
                                              (Left . phi)
                                              (Right . hoistAlt1 phi))
                                            x

altToAp :: Alt f a -> Ap (Coproduct f (Alt2 f)) a
altToAp (LiftAlt x) = liftAp (left x)
altToAp (Branch1 x) = ap'ToAp . unAlt1 $ x
altToAp (Branch2 x) = liftAp (right x)

apToAlt :: Functor f => Ap (Coproduct f (Alt2 f)) a -> Alt f a
apToAlt x = case apToAp' x of
  Left (Coproduct (Left l)) -> LiftAlt l
  Left (Coproduct (Right b2)) -> Branch2 b2
  Right b1 -> Branch1 (Alt1 b1)

altToList1 :: Alt f a -> List1 (Coproduct f (Alt1 f)) a
altToList1 (LiftAlt x) = liftList1 (left x)
altToList1 (Branch1 x) = liftList1 (right x)
altToList1 (Branch2 x) = list1'ToList1 . unAlt2 $ x

list1ToAlt :: List1 (Coproduct f (Alt1 f)) a -> Alt f a
list1ToAlt x = case list1ToList1' x of
  Left (Coproduct (Left l)) -> LiftAlt l
  Left (Coproduct (Right b1)) -> Branch1 b1
  Right b2 -> Branch2 (Alt2 b2)

instance Functor f => Functor (Alt f) where
  fmap f (LiftAlt x) = LiftAlt (fmap f x)
  fmap f (Branch1 b) = Branch1 (fmap f b)
  fmap f (Branch2 b) = Branch2 (fmap f b)

instance Functor f => Applicative (Alt f) where
  pure = Branch1 . pure
  f <*> x = apToAlt $ altToAp f <*> altToAp x

instance Functor f => Alternative (Alt f) where
  empty = Branch2 mempty1
  x <|> y = list1ToAlt $ mappend1 (altToList1 x) (altToList1 y)
