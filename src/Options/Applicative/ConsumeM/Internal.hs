{-# LANGUAGE Rank2Types #-}
module Options.Applicative.ConsumeM.Internal (
  ConsumeM(..),
  consumeAsk,
  consumeAbort
  ) where

import Control.Monad (ap)
import Prelude

-- | Internal consumer that can consume multiple arguments from the command line.
--
-- This type is parametric in the error type 'e', allowing different contexts
-- to provide appropriate error messages.
--
-- This type has full Functor/Applicative/Monad instances for internal use.
newtype ConsumeM e a = ConsumeM { runConsumeM :: [String] -> Either e (a, [String]) }

instance Functor (ConsumeM e) where
  fmap f (ConsumeM r) = ConsumeM $ \input ->
    case r input of
      Right (a, rest) -> Right (f a, rest)
      Left err -> Left err

instance Applicative (ConsumeM e) where
  pure a = ConsumeM $ \inputs -> Right (a, inputs)
  (<*>) = ap

instance Monad (ConsumeM e) where
  return = pure
  ConsumeM r >>= f = ConsumeM $ \inputs -> do
    (a, rest) <- r inputs
    runConsumeM (f a) rest

-- | Consume a single argument from the input list.
-- Returns the given error if no argument is available.
consumeAsk :: e -> ConsumeM e String
consumeAsk errMsg = ConsumeM $ \input -> case input of
  x:xs -> Right (x, xs)
  []   -> Left errMsg

-- | Abort consumption with an error.
consumeAbort :: e -> ConsumeM e a
consumeAbort e = ConsumeM $ \_ -> Left e
