module Options.Applicative.ConsumeA (
  -- * Types
  ConsumeA,

  -- * Combinators
  consumePair,
  consumeOne,
  consumeNone,

  -- * Internal
  unwrapConsumeA
  ) where

import Control.Monad.Trans.Except (runExcept)
import Control.Monad.Trans.Reader (runReaderT)
import Prelude

import qualified Options.Applicative.ConsumeA.Internal as CMI
import Options.Applicative.Types (ReadM(..), ParseError(..))

-- | An option parsing implementation that may consume multiple command-line arguments.
--
-- This type is intentionally abstract. Use 'consumePair', 'consumeOne', or
-- 'consumeNone' to construct consumers.
--
-- Unlike 'ReadM', which reads a single string value, 'ConsumeA' can consume
-- zero, one, or two arguments from the command line. This is useful for options like:
--
-- > --set key value
--
-- which take two separate arguments.
--
-- This cleanly avoids having to create a custom string format for pairwise arguments.
--
-- Note: This type is intentionally limited to avoid overly complex parsers.
newtype ConsumeA a = ConsumeA (CMI.ConsumeA ContextualError a)

-- | A ParseError that can make use of the real option name
type ContextualError = String -> ParseError

-- | Forward the Functor instance
instance Functor ConsumeA where
  fmap f (ConsumeA p) = ConsumeA (fmap f p)

-- | Unwrap a ConsumeA to get the internal ConsumeA implementation.
-- This is used internally by optparse-applicative to analyze and run the consumer.
unwrapConsumeA :: ConsumeA a -> CMI.ConsumeA ContextualError a
unwrapConsumeA (ConsumeA p) = p

-- | Consume exactly two arguments using the given readers.
--
-- The first 'ReadM' is applied to the first argument, and the second 'ReadM'
-- is applied to the second argument. The metavar strings are used in help text.
--
-- Example:
--
-- > import Options.Applicative
-- >
-- > setOption :: Parser (String, String)
-- > setOption = consumeOption (consumePair "KEY" str "VALUE" str)
-- >   ( long "set"
-- >  <> help "Set a configuration value" )
--
-- This will parse:
--
-- > --set name Alice
--
-- as @("name", "Alice")@ and display @--set KEY VALUE@ in help text.
consumePair :: String -> ReadM a -> String -> ReadM b -> ConsumeA (a, b)
consumePair metavar1 ra metavar2 rb = ConsumeA consumer
  where
    -- TODO: Custom error message would be nice for second argument
    --       Is it ok to extend the error type with more constructors?
    --       - The option `--foo` expects two arguments.
    --       - The option `--foo` expects a second argument.
    consumer =
      (,) <$> CMI.withMetavar metavar1 (consumeWith ra (ExpectsArgError))
          <*> CMI.withMetavar metavar2 (consumeWith rb (ExpectsArgError))

-- Specialise CMI.consumeAsk to ReadM (which would otherwise cause a cyclic import if done in CMI)
consumeWith :: ReadM x -> ContextualError -> CMI.ArgConsumer ContextualError x
consumeWith (ReadM r) err = do
  str <- CMI.consumeAsk (err)
  case runExcept (runReaderT r str) of
    Right x -> pure x
    Left parseError -> CMI.consumeAbort (const parseError)

-- | Consume exactly one argument using the given reader.
--
-- The 'ReadM' is applied to the argument to parse and validate it.
-- The metavar string is used in help text.
--
-- Example:
--
-- > import Options.Applicative
-- >
-- > outputOption :: Parser FilePath
-- > outputOption = consumeOption (consumeOne "FILE" str)
-- >   ( long "output"
-- >  <> help "Output file path" )
--
-- This will parse:
--
-- > --output results.txt
--
-- as @"results.txt"@ and display @--output FILE@ in help text.
consumeOne :: String -> ReadM a -> ConsumeA a
consumeOne metavar ra =
  ConsumeA (CMI.withMetavar metavar (consumeWith ra (ExpectsArgError)))

-- | Consume no arguments and return unit.
--
-- This allows using 'consumeOption' with the same uniform interface as
-- 'consumeOne' and 'consumePair', even when no arguments are needed.
--
-- For simple boolean flags, 'switch' is usually preferable: it returns
-- 'Parser Bool' directly with the default value built in, whereas
-- 'optional (consumeOption consumeNone ...)' returns 'Parser (Maybe ())'
-- which requires post-processing. However, 'consumeNone' is useful when
-- you want a uniform interface across options that consume different
-- numbers of arguments.
--
-- Example using the uniform 'consumeOption' interface:
--
-- > import Options.Applicative
-- > import qualified Options.Applicative.ConsumeA as ConsumeA
-- >
-- > -- Using the same builder for 0, 1, and 2 argument options
-- > data Options = Options
-- >   { verbose :: Maybe ()      -- 0 arguments
-- >   , output  :: Maybe FilePath -- 1 argument
-- >   , mapping :: [(String, String)] -- 2 arguments
-- >   }
-- >
-- > options :: Parser Options
-- > options = Options
-- >   <$> optional (consumeOption ConsumeA.consumeNone (long "verbose"))
-- >   <*> optional (consumeOption (ConsumeA.consumeOne "FILE" str) (long "output"))
-- >   <*> many (consumeOption (ConsumeA.consumePair "KEY" str "VALUE" str) (long "map"))
consumeNone :: ConsumeA ()
consumeNone = ConsumeA (pure ())
