{-# LANGUAGE Rank2Types, ExistentialQuantification #-}
module Options.Applicative.Types (
  ParseError(..),
  ParserPrefs(..),

  OptName(..),
  OptProperties(..),
  OptVisibility(..),
  ReadM(..),
  readerAsk,
  readerAbort,
  readerError,
  CReader(..),
  Completer(..),
  mkCompleter,
  CompletionResult(..),
  ParserFailure(..),
  ParserResult(..),
  overFailure,
  Args,
  ArgPolicy(..),
  OptHelpInfo(..),
  OptTree(..),
  ParserHelp(..),
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Except (Except, throwE)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Data.Monoid (Monoid(..))
import System.Exit (ExitCode(..))

import Options.Applicative.Help.Types
import Options.Applicative.Help.Pretty
import Options.Applicative.Help.Chunk

data ParseError
  = ErrorMsg String
  | InfoMsg String
  | ShowHelpText
  | UnknownError
  deriving Show

instance Monoid ParseError where
  mempty = UnknownError
  mappend m UnknownError = m
  mappend _ m = m

-- | Global preferences for a top-level 'Parser'.
data ParserPrefs = ParserPrefs
  { prefMultiSuffix :: String    -- ^ metavar suffix for multiple options
  , prefDisambiguate :: Bool     -- ^ automatically disambiguate abbreviations
                                 -- (default: False)
  , prefShowHelpOnError :: Bool  -- ^ always show help text on parse errors
                                 -- (default: False)
  , prefBacktrack :: Bool        -- ^ backtrack to parent parser when a
                                 -- subcommand fails (default: True)
  , prefColumns :: Int           -- ^ number of columns in the terminal, used to
                                 -- format the help page (default: 80)
  }

data OptName = OptShort !Char
             | OptLong !String
  deriving (Eq, Ord)

instance Pretty OptName where
  pretty (OptShort c) = string $ '-' : [c]
  pretty (OptLong n) = string $ '-' : '-' : n

-- | Visibility of an option in the help text.
data OptVisibility
  = Internal          -- ^ does not appear in the help text at all
  | Hidden            -- ^ only visible in the full description
  | Visible           -- ^ visible both in the full and brief descriptions
  deriving (Eq, Ord)

-- | Specification for an individual parser option.
data OptProperties = OptProperties
  { propVisibility :: OptVisibility       -- ^ whether this flag is shown is the brief description
  , propHelp :: Chunk Doc                 -- ^ help text for this option
  , propMetaVar :: String                 -- ^ metavariable for this option
  , propShowDefault :: Maybe String       -- ^ what to show in the help text as the default
  }

instance Pretty OptProperties where
  pretty d = string (propMetaVar d)

-- | A newtype over 'ReaderT String Except', used by option readers.
newtype ReadM a = ReadM
  { unReadM :: ReaderT String (Except ParseError) a }

instance Functor ReadM where
  fmap f (ReadM r) = ReadM (fmap f r)

instance Applicative ReadM where
  pure = ReadM . pure
  ReadM x <*> ReadM y = ReadM $ x <*> y

instance Alternative ReadM where
  empty = mzero
  (<|>) = mplus

instance Monad ReadM where
  return = pure
  ReadM r >>= f = ReadM $ r >>= unReadM . f
  fail = readerError

instance MonadPlus ReadM where
  mzero = ReadM mzero
  mplus (ReadM x) (ReadM y) = ReadM $ mplus x y

-- | Return the value being read.
readerAsk :: ReadM String
readerAsk = ReadM ask

-- | Abort option reader by exiting with a 'ParseError'.
readerAbort :: ParseError -> ReadM a
readerAbort = ReadM . lift . throwE

-- | Abort option reader by exiting with an error message.
readerError :: String -> ReadM a
readerError = readerAbort . ErrorMsg

data CReader a = CReader
  { crCompleter :: Completer
  , crReader :: ReadM a }

instance Functor CReader where
  fmap f (CReader c r) = CReader c (fmap f r)

newtype Completer = Completer
  { runCompleter :: String -> IO [String] }

mkCompleter :: (String -> IO [String]) -> Completer
mkCompleter = Completer

instance Monoid Completer where
  mempty = Completer $ \_ -> return []
  mappend (Completer c1) (Completer c2) =
    Completer $ \s -> (++) <$> c1 s <*> c2 s

newtype CompletionResult = CompletionResult
  { execCompletion :: String -> IO String }

instance Show CompletionResult where
  showsPrec p _ = showParen (p > 10) $
    showString "CompletionResult _"

newtype ParserFailure h = ParserFailure
  { execFailure :: String -> (h, ExitCode, Int) }

instance Show h => Show (ParserFailure h) where
  showsPrec p (ParserFailure f)
    = showParen (p > 10)
    $ showString "ParserFailure "
    . showsPrec 11 (f "<program>")

instance Functor ParserFailure where
  fmap f (ParserFailure err) = ParserFailure $ \progn ->
    let (h, exit, cols) = err progn in (f h, exit, cols)

-- | Result of 'execParserPure'.
data ParserResult a
  = Success a
  | Failure (ParserFailure ParserHelp)
  | CompletionInvoked CompletionResult
  deriving Show

instance Functor ParserResult where
  fmap f (Success a) = Success (f a)
  fmap _ (Failure f) = Failure f
  fmap _ (CompletionInvoked c) = CompletionInvoked c

overFailure :: (ParserHelp -> ParserHelp)
            -> ParserResult a -> ParserResult a
overFailure f (Failure failure) = Failure $ fmap f failure
overFailure _ r = r

instance Applicative ParserResult where
  pure = Success
  Success f <*> r = fmap f r
  Failure f <*> _ = Failure f
  CompletionInvoked c <*> _ = CompletionInvoked c

instance Monad ParserResult where
  return = pure
  Success x >>= f = f x
  Failure f >>= _ = Failure f
  CompletionInvoked c >>= _ = CompletionInvoked c

type Args = [String]

data ArgPolicy
  = SkipOpts
  | AllowOpts
  deriving Eq

data OptHelpInfo = OptHelpInfo
  { hinfoMulti :: Bool
  , hinfoDefault :: Bool }

data OptTree a
  = Leaf a
  | MultNode [OptTree a]
  | AltNode [OptTree a]
  deriving Show
