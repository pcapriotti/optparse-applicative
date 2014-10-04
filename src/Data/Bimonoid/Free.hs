module Data.Bimonoid.Free
  ( FreeBM1(..)
  , FreeBM2(..)
  , FreeBM(..)
  ) where

import Control.Applicative
import Control.Monad
import Data.Bifunctor
import Data.Bimonoid
import Data.Monoid

data List' a
  = Nil'
  | Cons' a a [a]
  deriving (Eq, Ord, Read, Show)

list'ToList :: List' a -> [a]
list'ToList Nil' = []
list'ToList (Cons' x1 x2 xs) = x1 : x2 : xs

instance Functor List' where
  fmap _ Nil' = Nil'
  fmap f (Cons' x1 x2 xs) = Cons' (f x1) (f x2) (map f xs)

newtype FreeBM1 a = FreeBM1
  { unFreeBM1 :: List' (Either a (FreeBM2 a)) }
  deriving (Eq, Ord, Read, Show)

instance Functor FreeBM1 where
  fmap f = FreeBM1 . fmap (bimap f (fmap f)) . unFreeBM1

elems1 :: FreeBM1 a -> [Either a (FreeBM2 a)]
elems1 = list'ToList . unFreeBM1

newtype FreeBM2 a = FreeBM2
  { unFreeBM2 :: List' (Either a (FreeBM1 a)) }
  deriving (Eq, Ord, Read, Show)

instance Functor FreeBM2 where
  fmap f = FreeBM2 . fmap (bimap f (fmap f)) . unFreeBM2

elems2 :: FreeBM2 a -> [Either a (FreeBM1 a)]
elems2 = list'ToList . unFreeBM2

data FreeBM a
  = LiftBM a
  | BMProd (FreeBM1 a)
  | BMSum (FreeBM2 a)
  deriving (Eq, Ord, Read, Show)

instance Functor FreeBM where
  fmap f (LiftBM a) = LiftBM (f a)
  fmap f (BMProd x) = BMProd (fmap f x)
  fmap f (BMSum x) = BMSum (fmap f x)

instance Applicative FreeBM where
  pure = LiftBM
  (<*>) = ap

bind1 :: (a -> FreeBM b) -> FreeBM1 a -> FreeBM b
bind1 f xs = mconcat (map (either f (bind2 f)) (elems1 xs))

bind2 :: (a -> FreeBM b) -> FreeBM2 a -> FreeBM b
bind2 f xs = mconcat (map (either f (bind1 f)) (elems2 xs))

instance Monad FreeBM where
  return = LiftBM
  LiftBM x >>= f = f x
  BMProd p >>= f = bind1 f p
  BMSum s >>= f = bind2 f s

bmToProd :: FreeBM a -> [Either a (FreeBM2 a)]
bmToProd (LiftBM x) = [Left x]
bmToProd (BMProd b1) = elems1 b1
bmToProd (BMSum b2) = [Right b2]

prodToBM :: [Either a (FreeBM2 a)] -> FreeBM a
prodToBM [] = BMProd (FreeBM1 Nil')
prodToBM [Left x] = LiftBM x
prodToBM [Right b2] = BMSum b2
prodToBM (x1 : x2 : xs) = BMProd (FreeBM1 (Cons' x1 x2 xs))

instance Monoid (FreeBM a) where
  mempty = prodToBM []
  mappend x y = prodToBM $ bmToProd x ++ bmToProd y

bmToSum :: FreeBM a -> [Either a (FreeBM1 a)]
bmToSum (LiftBM x) = [Left x]
bmToSum (BMProd b1) = [Right b1]
bmToSum (BMSum b2) = elems2 b2

sumToBM :: [Either a (FreeBM1 a)] -> FreeBM a
sumToBM [] = BMSum (FreeBM2 Nil')
sumToBM [Left x] = LiftBM x
sumToBM [Right b1] = BMProd b1
sumToBM (x1 : x2 : xs) = BMSum (FreeBM2 (Cons' x1 x2 xs))

instance Bimonoid (FreeBM a) where
  zero = sumToBM []
  plus x y = sumToBM $ bmToSum x ++ bmToSum y
