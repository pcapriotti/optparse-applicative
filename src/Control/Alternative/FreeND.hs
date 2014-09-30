-- | Free non-distributive 'Alternative' functor.
module Control.Alternative.FreeND
  ( Alt(..)
  , liftAlt
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

liftAlt :: f a -> Alt f a
liftAlt = LiftAlt

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
