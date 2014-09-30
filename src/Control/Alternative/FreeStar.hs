-- | Free non-distributive 'Alternative' with a freely generated 'some'
-- operation.
--
-- Note that `some` does /not/ verify:
--
-- > some v = (:) <$> v <*> many v
module Control.Alternative.FreeStar
  ( Alt(..)
  , liftAlt
  ) where

import Control.Applicative
import qualified Control.Alternative.FreeND as ND
import Data.Functor.Coproduct
import Data.Functor.Kan.Lan

newtype Alt f a = Alt
  { unAlt :: ND.Alt (Coproduct f (Lan [] (Alt f))) a }

liftAlt :: f a -> Alt f a
liftAlt = Alt . ND.liftAlt . left

instance Functor f => Functor (Alt f) where
  fmap f = Alt . fmap f . unAlt

instance Functor f => Applicative (Alt f) where
  pure = Alt . pure
  f <*> x = Alt $ unAlt f <*> unAlt x

instance Functor f => Alternative (Alt f) where
  empty = Alt empty
  x <|> y = Alt $ unAlt x <|> unAlt y

  some x = Alt . ND.liftAlt . right . Lan id $ x
  many x = some x <|> pure []
