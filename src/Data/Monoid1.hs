{-# LANGUAGE RankNTypes #-}
module Data.Monoid1
  ( Monoid1(..)
  , List1(..)
  , liftList1
  , runList1
  , runList1Alt
  , List1'(..)
  , list1'ToList1
  , list1ToList1'
  ) where

import Control.Applicative
import Data.Foldable (asum)

class Functor f => Monoid1 f where
  mempty1 :: f a
  mappend1 :: f a -> f a -> f a

newtype List1 f a = List1
  { unList1 :: [f a] }
  deriving (Eq, Ord, Read, Show)

liftList1 :: f a -> List1 f a
liftList1 = List1 . pure

runList1 :: Monoid1 g => (forall x . f x -> g x) -> List1 f a -> g a
runList1 f (List1 xs) = foldr mappend1 mempty1 (map f xs)

runList1Alt :: Alternative g => (forall x . f x -> g x) -> List1 f a -> g a
runList1Alt f (List1 xs) = asum (map f xs)

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

list1'ToList1 :: List1' f a -> List1 f a
list1'ToList1 Nil' = List1 []
list1'ToList1 (Cons' x1 x2 (List1 xs)) = List1 (x1 : x2 : xs)

list1ToList1' :: List1 f a -> Either (f a) (List1' f a)
list1ToList1' (List1 []) = Right Nil'
list1ToList1' (List1 [x]) = Left x
list1ToList1' (List1 (x : y : xs)) = Right (Cons' x y (List1 xs))

instance Functor f => Functor (List1' f) where
  fmap _ Nil' = Nil'
  fmap f (Cons' x1 x2 xs) = Cons' (fmap f x1) (fmap f x2) (fmap f xs)

instance Functor f => Monoid1 (List1' f) where
  mempty1 = Nil'
  mappend1 Nil' x = x
  mappend1 (Cons' x1 x2 xs) ys = Cons' x1 x2 (mappend1 xs (list1'ToList1 ys))
