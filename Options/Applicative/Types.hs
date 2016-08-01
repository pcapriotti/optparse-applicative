{-# LANGUAGE Rank2Types, ExistentialQuantification #-}
module Options.Applicative.Types (
  ParseError(..),
  ParserInfo(..),
  ParserPrefs(..),

  Option(..),
  OptName(..),
  OptReader(..),
  OptProperties(..),
  OptVisibility(..),
  ReadM(..),
  readerAsk,
  readerAbort,
  readerError,
  CReader(..),
  Parser(..),
  ParserM(..),
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
  SomeParser(..),
  Context(..),
  IsCmdStart(..),

  fromM,
  oneM,
  manyM,
  someM,

  optVisibility,
  optMetaVar,
  optHelp,
  optShowDefault
  ) where

import Control.Applicative
import Control.Monad (ap, liftM, MonadPlus, mzero, mplus)
import Control.Monad.Trans.Except (Except, throwE)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Data.Semigroup hiding (Option)
import Prelude

import System.Exit (ExitCode(..))

import Options.Applicative.Help.Types
import Options.Applicative.Help.Pretty
import Options.Applicative.Help.Chunk


data ParseError
  = ErrorMsg String
  | InfoMsg String
  | ShowHelpText
  | UnknownError
  | MissingError IsCmdStart SomeParser

data IsCmdStart = CmdStart | CmdCont
  deriving Show

instance Monoid ParseError where
  mempty = UnknownError
  mappend = (<>)

instance Semigroup ParseError where
  m <> UnknownError = m
  _ <> m = m

-- | A full description for a runnable 'Parser' for a program.
data ParserInfo a = ParserInfo
  { infoParser :: Parser a    -- ^ the option parser for the program
  , infoFullDesc :: Bool      -- ^ whether the help text should contain full
                              -- documentation
  , infoProgDesc :: Chunk Doc -- ^ brief parser description
  , infoHeader :: Chunk Doc   -- ^ header of the full parser description
  , infoFooter :: Chunk Doc   -- ^ footer of the full parser description
  , infoFailureCode :: Int    -- ^ exit code for a parser failure
  , infoIntersperse :: Bool   -- ^ allow regular options and flags to occur
                              -- after arguments (default: True)
  }

instance Functor ParserInfo where
  fmap f i = i { infoParser = fmap f (infoParser i) }

-- | Global preferences for a top-level 'Parser'.
data ParserPrefs = ParserPrefs
  { prefMultiSuffix :: String    -- ^ metavar suffix for multiple options
  , prefDisambiguate :: Bool     -- ^ automatically disambiguate abbreviations
                                 -- (default: False)
  , prefShowHelpOnError :: Bool  -- ^ always show help text on parse errors
                                 -- (default: False)
  , prefShowHelpOnEmpty :: Bool  -- ^ show the help text for a command or subcommand
                                 -- if it fails with no input (default: False)
  , prefBacktrack :: Bool        -- ^ backtrack to parent parser when a
                                 -- subcommand fails (default: True)
  , prefColumns :: Int           -- ^ number of columns in the terminal, used to
                                 -- format the help page (default: 80)
  } deriving (Eq, Show)

data OptName = OptShort !Char
             | OptLong !String
  deriving (Eq, Ord, Show)

-- | Visibility of an option in the help text.
data OptVisibility
  = Internal          -- ^ does not appear in the help text at all
  | Hidden            -- ^ only visible in the full description
  | Visible           -- ^ visible both in the full and brief descriptions
  deriving (Eq, Ord, Show)

-- | Specification for an individual parser option.
data OptProperties = OptProperties
  { propVisibility :: OptVisibility       -- ^ whether this flag is shown is the brief description
  , propHelp :: Chunk Doc                 -- ^ help text for this option
  , propMetaVar :: String                 -- ^ metavariable for this option
  , propShowDefault :: Maybe String       -- ^ what to show in the help text as the default
  } deriving Show

-- | A single option of a parser.
data Option a = Option
  { optMain :: OptReader a               -- ^ reader for this option
  , optProps :: OptProperties            -- ^ properties of this option
  }

data SomeParser = forall a . SomeParser (Parser a)

-- | Subparser context, containing the 'name' of the subparser, and its parser info.
--   Used by parserFailure to display relevant usage information when parsing inside a subparser fails.
data Context = forall a . Context String (ParserInfo a)

instance Show (Option a) where
    show opt = "Option {optProps = " ++ show (optProps opt) ++ "}"

instance Functor Option where
  fmap f (Option m p) = Option (fmap f m) p

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

-- | An 'OptReader' defines whether an option matches an command line argument.
data OptReader a
  = OptReader [OptName] (CReader a) ParseError          -- ^ option reader
  | FlagReader [OptName] !a                             -- ^ flag reader
  | ArgReader (CReader a)                               -- ^ argument reader
  | CmdReader (Maybe String)
              [String] (String -> Maybe (ParserInfo a)) -- ^ command reader

instance Functor OptReader where
  fmap f (OptReader ns cr e) = OptReader ns (fmap f cr) e
  fmap f (FlagReader ns x) = FlagReader ns (f x)
  fmap f (ArgReader cr) = ArgReader (fmap f cr)
  fmap f (CmdReader n cs g) = CmdReader n cs ((fmap . fmap) f . g)

-- | A @Parser a@ is an option parser returning a value of type 'a'.
data Parser a
  = NilP (Maybe a)
  | OptP (Option a)
  | forall x . MultP (Parser (x -> a)) (Parser x)
  | AltP (Parser a) (Parser a)
  | forall x . BindP (Parser x) (x -> Parser a)

instance Functor Parser where
  fmap f (NilP x) = NilP (fmap f x)
  fmap f (OptP opt) = OptP (fmap f opt)
  fmap f (MultP p1 p2) = MultP (fmap (f.) p1) p2
  fmap f (AltP p1 p2) = AltP (fmap f p1) (fmap f p2)
  fmap f (BindP p k) = BindP p (fmap f . k)

instance Applicative Parser where
  pure = NilP . Just
  (<*>) = MultP

newtype ParserM r = ParserM
  { runParserM :: forall x . (r -> Parser x) -> Parser x }

instance Monad ParserM where
  return x = ParserM $ \k -> k x
  ParserM f >>= g = ParserM $ \k -> f (\x -> runParserM (g x) k)

instance Functor ParserM where
  fmap = liftM

instance Applicative ParserM where
  pure = return
  (<*>) = ap

fromM :: ParserM a -> Parser a
fromM (ParserM f) = f pure

oneM :: Parser a -> ParserM a
oneM p = ParserM (BindP p)

manyM :: Parser a -> ParserM [a]
manyM p = do
  mx <- oneM (optional p)
  case mx of
    Nothing -> return []
    Just x -> (x:) <$> manyM p

someM :: Parser a -> ParserM [a]
someM p = (:) <$> oneM p <*> manyM p

instance Alternative Parser where
  empty = NilP Nothing
  (<|>) = AltP
  many p = fromM $ manyM p
  some p = fromM $ (:) <$> oneM p <*> manyM p

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

-- | Policy for how to handle options within the parse
data ArgPolicy
  = SkipOpts  -- ^ Inputs beginning with `-` or `--` are treated
              --   as options or flags, and can be mixed with arguments.
  | AllowOpts -- ^ All input is treated as positional arguments.
              --   Used after a bare `--` input, and also with
              --   `noIntersperse` policy.
  deriving (Eq, Show)

data OptHelpInfo = OptHelpInfo
  { hinfoMulti :: Bool
  , hinfoDefault :: Bool
  } deriving (Eq, Show)

data OptTree a
  = Leaf a
  | MultNode [OptTree a]
  | AltNode [OptTree a]
  deriving Show

optVisibility :: Option a -> OptVisibility
optVisibility = propVisibility . optProps

optHelp :: Option a -> Chunk Doc
optHelp  = propHelp . optProps

optMetaVar :: Option a -> String
optMetaVar = propMetaVar . optProps

optShowDefault :: Option a -> Maybe String
optShowDefault = propShowDefault . optProps
