{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | An approximation of the free 'Bimonoid'.
module Data.Bimonoid.Expr
  ( Expr(..)
  , Expr'(..)
  , runExpr
  , runExpr'
  , StarExpr(..)
  , runStarExpr
  ) where

import Control.Applicative
import Control.Monad
import Data.Bimonoid
import Data.Bifunctor
import Data.Semigroup
import Prelude hiding (sum)

-- | An expression built using two associative operations.
--
-- This is a 'Bimonoid', but not strictly a free one.  However, there is a
-- retraction from 'Expr a' to the free bimonoid on 'a'.
data Expr a
  = Lit a                 -- ^ a literal
  | ProdExpr [Expr' a]    -- ^ a product of expressions

data Expr' a
  = Lit' a
  | SumExpr [Expr a]

runExpr :: Bimonoid a => Expr a -> a
runExpr (Lit x) = x
runExpr (ProdExpr ts) = foldr mappend mempty (map runExpr' ts)

runExpr' :: Bimonoid a => Expr' a -> a
runExpr' (Lit' x) = x
runExpr' (SumExpr ts) = foldr plus zero (map runExpr ts)

instance Functor Expr where
  fmap f (Lit x) = Lit (f x)
  fmap f (ProdExpr ts) = ProdExpr (map (fmap f) ts)

instance Functor Expr' where
  fmap f (Lit' x) = Lit' (f x)
  fmap f (SumExpr ts) = SumExpr (map (fmap f) ts)

instance Applicative Expr where
  pure = Lit
  (<*>) = ap

instance Monad Expr where
  return = Lit
  e >>= f = runExpr (fmap f e)

-- terms . prod = id

terms :: Expr a -> [Expr' a]
terms (Lit x) = [Lit' x]
terms (ProdExpr ts) = ts

prod :: [Expr' a] -> Expr a
prod [Lit' x] = Lit x
prod ts = ProdExpr ts

instance Monoid (Expr a) where
  mempty = ProdExpr []
  mappend (ProdExpr []) y = y
  mappend x (ProdExpr []) = x
  mappend x y = prod $ terms x ++ terms y

-- terms' . sum = id

terms' :: Expr a -> [Expr a]
terms' (ProdExpr [SumExpr ts]) = ts
terms' x = [x]

sum :: [Expr a] -> Expr a
sum [e@(Lit _)] = e
sum [e@(ProdExpr [])] = e
sum [e@(ProdExpr (_:_:_))] = e
sum ts = ProdExpr [SumExpr ts]

instance Bimonoid (Expr a) where
  zero = ProdExpr [SumExpr []]
  plus (ProdExpr [SumExpr []]) y = y
  plus x (ProdExpr [SumExpr []]) = x
  plus x y = sum $ terms' x ++ terms' y

-- | An expression built with two associative operations and a unary "star"
-- operation.
newtype StarExpr a = StarExpr
  { unStarExpr :: Expr (Either (StarExpr a) a) }

runStarExpr :: StarBimonoid a => StarExpr a -> a
runStarExpr = runExpr . fmap (either runStarExpr id) . unStarExpr

instance Functor StarExpr where
  fmap f = StarExpr . fmap (bimap (fmap f) f) . unStarExpr

instance Applicative StarExpr where
  pure = StarExpr . pure . Right
  (<*>) = ap

instance Monad StarExpr where
  return = pure
  m >>= f = runStarExpr (fmap f m)

instance Monoid (StarExpr a) where
  mempty = StarExpr mempty
  mappend x y = StarExpr $ mappend (unStarExpr x) (unStarExpr y)

instance Bimonoid (StarExpr a) where
  zero = StarExpr zero
  plus x y = StarExpr $ plus (unStarExpr x) (unStarExpr y)

instance StarBimonoid (StarExpr a) where
  star = StarExpr . pure . Left
