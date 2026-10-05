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
import Options.Applicative.Types (ReadM(..), ParseError(..), Completer(..))
import Options.Applicative.Builder.Internal (Mod(..), OptionArgumentFields(..))

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
newtype ConsumeA a = ConsumeA (CMI.ConsumeA ([String], [Completer]) ContextualError a)

-- | A ParseError that can make use of the real option name
type ContextualError = String -> ParseError

-- | Forward the Functor instance
instance Functor ConsumeA where
  fmap f (ConsumeA p) = ConsumeA (fmap f p)

-- | Unwrap a ConsumeA to get the internal ConsumeA implementation.
-- This is used internally by optparse-applicative to analyze and run the consumer.
unwrapConsumeA :: ConsumeA a -> CMI.ConsumeA ([String], [Completer]) ContextualError a
unwrapConsumeA (ConsumeA p) = p

-- | Attach a metavar and completer to an argument consumer.
-- This creates the metadata pair ([metavar], [completer]) for the Writer pattern.
withMetavar :: String -> Completer -> CMI.ArgConsumer ContextualError a -> CMI.ConsumeA ([String], [Completer]) ContextualError a
withMetavar metavar completer consumer = CMI.makeConsumeA ([metavar], [completer]) consumer

-- | Consume exactly two arguments using the given readers.
--
-- The first 'ReadM' is applied to the first argument, and the second 'ReadM'
-- is applied to the second argument.
-- Use 'metavar' to specify the metavar string for each argument (defaults to "ARG").
-- Use 'completer' to specify shell completion behavior for each argument.
--
-- Example:
--
-- > import Options.Applicative
-- >
-- > setOption :: Parser (String, String)
-- > setOption = consumeOption
-- >   (consumePair str (metavar "KEY") str (metavar "VALUE"))
-- >   ( long "set"
-- >  <> help "Set a configuration value" )
--
-- This will parse:
--
-- > --set name Alice
--
-- as @("name", "Alice")@ and display @--set KEY VALUE@ in help text.
consumePair :: ReadM a -> Mod OptionArgumentFields a
            -> ReadM b -> Mod OptionArgumentFields b
            -> ConsumeA (a, b)
consumePair readerA (Mod fA _dA _gA) readerB (Mod fB _dB _gB) = ConsumeA consumer
  where
    OptionArgumentFields complA mvA = fA (OptionArgumentFields mempty "ARG")
    OptionArgumentFields complB mvB = fB (OptionArgumentFields mempty "ARG")
    -- TODO: Custom error message would be nice for second argument
    --       Is it ok to extend the error type with more constructors?
    --       - The option `--foo` expects two arguments.
    --       - The option `--foo` expects a second argument.
    consumer =
      (,) <$> consumeWithCReader mvA complA readerA (ExpectsArgError)
          <*> consumeWithCReader mvB complB readerB (ExpectsArgError)

-- | Helper to consume using a ReadM with metavar and completer, tracking both
consumeWithCReader :: String -> Completer -> ReadM x -> ContextualError -> CMI.ConsumeA ([String], [Completer]) ContextualError x
consumeWithCReader metavar completer (ReadM r) err =
  withMetavar metavar completer $ do
    str <- CMI.consumeAsk err
    case runExcept (runReaderT r str) of
      Right x -> pure x
      Left parseError -> CMI.consumeAbort (const parseError)

-- | Consume exactly one argument using the given reader.
--
-- The 'ReadM' is applied to the argument to parse and validate it.
-- Use 'metavar' to specify the metavar string for help text (defaults to "ARG").
-- Use 'completer' to specify shell completion behavior.
--
-- Example:
--
-- > import Options.Applicative
-- >
-- > outputOption :: Parser FilePath
-- > outputOption = consumeOption
-- >   (consumeOne str (metavar "FILE"))
-- >   ( long "output"
-- >  <> help "Output file path" )
--
-- This will parse:
--
-- > --output results.txt
--
-- as @"results.txt"@ and display @--output FILE@ in help text.
consumeOne :: ReadM a -> Mod OptionArgumentFields a -> ConsumeA a
consumeOne reader (Mod f _d _g) =
  ConsumeA (consumeWithCReader mv compl reader (ExpectsArgError))
  where
    OptionArgumentFields compl mv = f (OptionArgumentFields mempty "ARG")

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
-- >   <*> optional (consumeOption (ConsumeA.consumeOne str (metavar "FILE")) (long "output"))
-- >   <*> many (consumeOption (ConsumeA.consumePair str (metavar "KEY") str (metavar "VALUE")) (long "map"))
consumeNone :: ConsumeA ()
consumeNone = ConsumeA (pure ())

