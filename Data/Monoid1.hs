module Data.Monoid1
  ( Monoid1(..)
  , List1(..)
  , List1'(..)
  ) where

import Control.Applicative

class Functor f => Monoid1 f where
  mempty1 :: f a
  mappend1 :: f a -> f a -> f a

newtype List1 f a = List1
  { unList1 :: [f a] }
  deriving (Eq, Ord, Read, Show)

instance Functor f => Functor (List1 f) where
  fmap f = List1 . map (fmap f) . unList1

instance Functor f => Monoid1 (List1 f) where
  mempty1 = List1 []
  mappend1 xs ys = List1 $ unList1 xs ++ unList1 ys

instance Applicative f => Applicative (List1 f) where
  pure = List1 . pure . pure
  fs <*> xs = List1 $ (<*>) <$> unList1 fs <*> unList1 xs

instance Applicative f => Alternative (List1 f) where
  empty = mempty1
  (<|>) = mappend1

data List1' f a
  = Nil'
  | Cons' (f a) (f a) (List1 f a)

instance Functor f => Functor (List1' f) where
  fmap _ Nil' = Nil'
  fmap f (Cons' x1 x2 xs) = Cons' (fmap f x1) (fmap f x2) (fmap f xs)
