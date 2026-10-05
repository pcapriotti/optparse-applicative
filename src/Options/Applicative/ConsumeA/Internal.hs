module Options.Applicative.ConsumeA.Internal (
  -- * Argument consumption
  ArgConsumer(..),
  consumeAsk,
  consumeAbort,

  -- * Metadata tracking: consumer + writer
  ConsumeA,
  runConsumeA,
  makeConsumeA
  ) where

import Control.Monad (ap)
import Data.Functor.Compose
import Prelude

-- | The actual argument consumer that processes command-line arguments.
--
-- This type handles the parsing logic: consuming arguments from a list
-- and returning either an error or a result with remaining arguments.
newtype ArgConsumer e a = ArgConsumer { runArgConsumer :: [String] -> Either e (a, [String]) }

instance Functor (ArgConsumer e) where
  fmap f (ArgConsumer r) = ArgConsumer $ \input ->
    case r input of
      Right (a, rest) -> Right (f a, rest)
      Left err -> Left err

instance Applicative (ArgConsumer e) where
  pure a = ArgConsumer $ \inputs -> Right (a, inputs)
  (<*>) = ap

instance Monad (ArgConsumer e) where
  return = pure
  ArgConsumer r >>= f = ArgConsumer $ \inputs -> do
    (a, rest) <- r inputs
    runArgConsumer (f a) rest

-- | Consume a single argument from the input list.
-- Returns the given error if no argument is available.
consumeAsk :: e -> ArgConsumer e String
consumeAsk errMsg = ArgConsumer $ \input -> case input of
  x:xs -> Right (x, xs)
  []   -> Left errMsg

-- | Abort consumption with an error.
consumeAbort :: e -> ArgConsumer e a
consumeAbort e = ArgConsumer $ \_ -> Left e

-- | Generic consumer that accumulates metadata using Writer behavior.
--
-- The structure is @Compose ((,) w) (ArgConsumer e) a@ which expands to
-- @(w, ArgConsumer e a)@ where:
-- - @w@ accumulates metadata (e.g., metavars and completers) using Writer behavior
-- - ArgConsumer e a handles the actual argument consumption
--
-- By using 'Compose', the Functor and Applicative instances are automatically
-- derived, combining the monoidal accumulation of metadata with the applicative
-- behavior of ArgConsumer.
type ConsumeA w e a =
  -- Note: The types w and e are not needed for flexibility but to avoid import cycles
  Compose ((,) w) (ArgConsumer e) a

-- | Run a ConsumeA to extract both the metadata and the argument consumer.
runConsumeA :: ConsumeA w e a -> (w, ArgConsumer e a)
runConsumeA (Compose pair) = pair

-- | Construct a ConsumeA from metadata and an argument consumer.
makeConsumeA :: w -> ArgConsumer e a -> ConsumeA w e a
makeConsumeA metadata consumer = Compose (metadata, consumer)
