module Options.Applicative.ConsumeM (
  -- * Types
  ConsumeM,

  -- * Combinators
  consumePair,
  consumeOne,
  consumeNone,

  -- * Internal
  unwrapConsumeM
  ) where

import Control.Monad.Trans.Except (runExcept)
import Control.Monad.Trans.Reader (runReaderT)
import Prelude

import qualified Options.Applicative.ConsumeM.Internal as CMI
import Options.Applicative.Types (ReadM(..), ParseError(..))

-- | A consumer that consumes multiple command-line arguments.
--
-- This type is intentionally abstract. Use 'consumePair', 'consumeOne', or
-- 'consumeNone' to construct consumers.
--
-- Unlike 'ReadM', which reads a single string value, 'ConsumeM' can consume
-- zero, one, or two arguments from the command line. This is useful for options like:
--
-- > --set key value
--
-- which take two separate arguments.
--
-- Note: This type only exposes a 'Functor' instance. The 'Applicative' and
-- 'Monad' instances are intentionally hidden to prevent accidentally creating
-- consumers that consume an unbounded number of arguments.
newtype ConsumeM a = ConsumeM { unConsumeM :: CMI.ConsumeM ParseError a }

-- | Forward the Functor instance
instance Functor ConsumeM where
  fmap f (ConsumeM p) = ConsumeM (fmap f p)

-- | Unwrap a ConsumeM to get the internal ConsumeM. This is used internally
-- by the library and should not be exposed to end users.
unwrapConsumeM :: ConsumeM a -> CMI.ConsumeM ParseError a
unwrapConsumeM (ConsumeM p) = p

-- | Consume exactly two arguments using the given readers.
--
-- The first 'ReadM' is applied to the first argument, and the second 'ReadM'
-- is applied to the second argument.
--
-- Example:
--
-- > import Options.Applicative
-- >
-- > setOption :: Parser (String, String)
-- > setOption = consumeOption (consumePair str str)
-- >   ( long "set"
-- >  <> metavar "KEY VALUE"
-- >  <> help "Set a configuration value" )
--
-- This will parse:
--
-- > --set name Alice
--
-- as @("name", "Alice")@.
consumePair :: ReadM a -> ReadM b -> ConsumeM (a, b)
consumePair ra rb = ConsumeM $ do
  a <- consumeWith ra (ErrorMsg "expected first argument")
  b <- consumeWith rb (ErrorMsg "expected second argument")
  pure (a, b)
  where
    consumeWith :: ReadM x -> ParseError -> CMI.ConsumeM ParseError x
    consumeWith (ReadM r) errMsg = do
      str <- CMI.consumeAsk errMsg
      case runExcept (runReaderT r str) of
        Right x -> pure x
        Left parseError -> CMI.consumeAbort parseError

-- | Consume exactly one argument using the given reader.
--
-- The 'ReadM' is applied to the argument to parse and validate it.
--
-- Example:
--
-- > import Options.Applicative
-- >
-- > outputOption :: Parser FilePath
-- > outputOption = consumeOption (consumeOne str)
-- >   ( long "output"
-- >  <> metavar "FILE"
-- >  <> help "Output file path" )
--
-- This will parse:
--
-- > --output results.txt
--
-- as @"results.txt"@.
consumeOne :: ReadM a -> ConsumeM a
consumeOne ra = ConsumeM $ consumeWith ra (ErrorMsg "expected argument")
  where
    consumeWith :: ReadM x -> ParseError -> CMI.ConsumeM ParseError x
    consumeWith (ReadM r) errMsg = do
      str <- CMI.consumeAsk errMsg
      case runExcept (runReaderT r str) of
        Right x -> pure x
        Left parseError -> CMI.consumeAbort parseError

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
-- > import qualified Options.Applicative.ConsumeM as ConsumeM
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
-- >   <$> optional (consumeOption ConsumeM.consumeNone (long "verbose"))
-- >   <*> optional (consumeOption (ConsumeM.consumeOne str) (long "output"))
-- >   <*> many (consumeOption (ConsumeM.consumePair str str) (long "map"))
consumeNone :: ConsumeM ()
consumeNone = ConsumeM $ pure ()
