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

-- | Metadata pair: metavars and completers (opaque type to avoid exporting implementation details)
-- Parameterized by c to avoid circular imports with Types.hs
type MetadataPair c = ([String], [c])

-- | Internal consumer that accumulates metavars and completers using Writer behavior.
--
-- The structure is @Compose ((,) ([String], [c])) (ArgConsumer e) a@ which expands to
-- @(([String], [c]), ArgConsumer e a)@ where:
-- - ([String], [c]) accumulates metavar names and completers for help/completion (Writer behavior)
-- - ArgConsumer e a handles the actual argument consumption
--
-- By using 'Compose', the Functor and Applicative instances are automatically
-- derived, combining the monoidal accumulation of metadata with the applicative
-- behavior of ArgConsumer.
--
-- The 'c' parameter avoids circular imports: Types.hs imports ConsumeA.Internal,
-- so Internal can't import Completer from Types. The consumer instantiates it as Completer.
type ConsumeA c e a = Compose ((,) (MetadataPair c)) (ArgConsumer e) a

-- | Run a ConsumeA to extract both the metavars, completers, and the argument consumer.
runConsumeA :: ConsumeA c e a -> (MetadataPair c, ArgConsumer e a)
runConsumeA (Compose pair) = pair

withMetavar :: String -> c -> ArgConsumer e a -> ConsumeA c e a
withMetavar mv completer p = Compose (([mv], [completer]), p)
