module Options.Applicative.ConsumeA.Internal (
  -- * Argument consumption
  ArgConsumer(..),
  consumeAsk,
  consumeAbort,

  -- * Metavar tracking
  ConsumeA,
  runConsumeA,
  withMetavar
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

-- | Internal consumer that accumulates metavars using Writer behavior.
--
-- The structure is @Compose ((,) [String]) (ArgConsumer e) a@ which expands to
-- @([String], ArgConsumer e a)@ where:
-- - [String] accumulates metavar names for help text generation (Writer behavior)
-- - ArgConsumer e a handles the actual argument consumption
--
-- By using 'Compose', the Functor and Applicative instances are automatically
-- derived, combining the monoidal accumulation of metavars with the applicative
-- behavior of ArgConsumer.
type ConsumeA e a = Compose ((,) [String]) (ArgConsumer e) a

-- | Run a ConsumeA to extract both the metavars and the argument consumer.
runConsumeA :: ConsumeA e a -> ([String], ArgConsumer e a)
runConsumeA (Compose pair) = pair

withMetavar :: String -> ArgConsumer e a -> ConsumeA e a
withMetavar mv p = Compose ([mv], p)
