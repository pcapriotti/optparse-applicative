{-# LANGUAGE RankNTypes #-}
-- | Free non-distributive 'Alternative' with a freely generated 'some'
-- operation.
--
-- Note that `some` does /not/ verify:
--
-- > some v = (:) <$> v <*> many v
module Control.Alternative.FreeStar
  ( Alt(..)
  , liftAlt
  , runAlt
  , hoistAlt
  ) where

import Control.Applicative
import qualified Control.Alternative.FreeND as ND
import Data.Functor.Coproduct
import Data.Functor.Kan.Lan

newtype Alt f a = Alt
  { unAlt :: ND.Alt (Coproduct f (Lan [] (Alt f))) a }

liftAlt :: f a -> Alt f a
liftAlt = Alt . ND.liftAlt . left

runAlt :: (Functor f, Alternative g) => (forall x . f x -> g x) -> Alt f a -> g a
runAlt f (Alt x) = ND.runAlt (coproduct f (toLan (some . runAlt f))) x

hoistAlt :: (forall a. f a -> g a) -> Alt f b -> Alt g b
hoistAlt phi (Alt x) = Alt $ ND.hoistAlt (Coproduct . coproduct
                                            (Left . phi)
                                            (Right . hoistLan (hoistAlt phi)))
                                          x
  where
    hoistLan :: (forall x. h x -> k x) -> Lan g h a -> Lan g k a
    hoistLan phi' (Lan s t) = Lan s (phi' t)

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
