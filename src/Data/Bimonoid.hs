module Data.Bimonoid
  ( Bimonoid(..)
  , StarBimonoid(..)
  , BConst(..)
  ) where

import Control.Applicative
import Data.Monoid

-- | A 'Bimonoid' is simply a type with two monoid structures.
--
-- The two instances are not required to satisfy any distributivity law. By
-- convention, the 'Monoid' operations are thought of as "multiplicative", while
-- the extra operations contained in 'Bimonoid' are "additive".
--
-- > plus zero y = y
-- > plus x zero = x
-- > plus x (plus y z) = plus (plus x y) z
class Monoid m => Bimonoid m where
  zero :: m
  plus :: m -> m -> m

-- | A 'Bimonoid' with an additional 'star' operation.
--
-- A 'StarBimonoid' is an approximation of a star semiring or Kleene algebra,
-- where the two operations are not required to distribute, and the addition is
-- not necessarily idempotent.
--
-- The 'star' operation does not satisfy any law.
class Bimonoid m => StarBimonoid m where
  star :: m -> m

-- | Same as 'Const', but with a extra 'Alternative' instance.
newtype BConst m x = BConst
  { getBConst :: m }

coerce :: BConst m x -> BConst m y
coerce = BConst . getBConst

instance Functor (BConst m) where
  fmap _ = coerce

instance Monoid m => Applicative (BConst m) where
  pure = BConst . mempty
  f <*> x = BConst $ getBConst f `mappend` getBConst x

instance StarBimonoid m => Alternative (BConst m) where
  empty = BConst zero
  x <|> y = BConst $ getBConst x `plus` getBConst y
