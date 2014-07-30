{-# LANGUAGE GADTs, Rank2Types #-}
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
  Args,
  ArgPolicy(..),
  OptHelpInfo(..),
  OptTree(..),

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
  (Applicative(..), Alternative(..), (<$>), optional)
import Control.Monad (ap, liftM, MonadPlus, mzero, mplus)
import Data.Monoid (Monoid(..))
import System.Exit (ExitCode(..))

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
  mappend UnknownError m = m
  mappend m _ = m

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
  , prefBacktrack :: Bool        -- ^ backtrack to parent parser when a
                                 -- subcommand fails (default: True)
  , prefColumns :: Int           -- ^ number of columns in the terminal, used to
                                 -- format the help page (default: 80)
  }

data OptName = OptShort !Char
             | OptLong !String
  deriving (Eq, Ord)

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

-- | A single option of a parser.
data Option a = Option
  { optMain :: OptReader a               -- ^ reader for this option
  , optProps :: OptProperties            -- ^ properties of this option
  }

instance Functor Option where
  fmap f (Option m p) = Option (fmap f m) p

data CReader m a = CReader
  { crCompleter :: Completer
  , crReader :: String -> m a }

instance Functor m => Functor (CReader m) where
  fmap f (CReader c r) = CReader c (fmap f . r)

-- | A newtype over the 'Either' monad used by option readers.
newtype ReadM a = ReadM
  { runReadM :: Either ParseError a }

instance Functor ReadM where
  fmap f (ReadM m) = ReadM (fmap f m)

instance Applicative ReadM where
  pure = ReadM . Right
  ReadM b <*> ReadM a = ReadM (b <*> a)

instance Monad ReadM where
  return = ReadM . Right
  ReadM m >>= f = ReadM $ m >>= runReadM . f
  fail = ReadM . Left . ErrorMsg

instance MonadPlus ReadM where
  mzero = ReadM $ Left UnknownError
  mplus m1 m2 = case runReadM m1 of
    Left _ -> m2
    Right r -> return r

-- | Abort option reader by exiting with a 'ParseError'.
readerAbort :: ParseError -> ReadM a
readerAbort = ReadM . Left

-- | Abort option reader by exiting with an error message.
readerError :: String -> ReadM a
readerError = readerAbort . ErrorMsg

type OptCReader = CReader ReadM
type ArgCReader = CReader Maybe

-- | An 'OptReader' defines whether an option matches an command line argument.
data OptReader a
  = OptReader [OptName] (OptCReader a) ParseError       -- ^ option reader
  | FlagReader [OptName] !a                             -- ^ flag reader
  | ArgReader (ArgCReader a)                            -- ^ argument reader
  | CmdReader [String] (String -> Maybe (ParserInfo a)) -- ^ command reader

instance Functor OptReader where
  fmap f (OptReader ns cr e) = OptReader ns (fmap f cr) e
  fmap f (FlagReader ns x) = FlagReader ns (f x)
  fmap f (ArgReader cr) = ArgReader (fmap f cr)
  fmap f (CmdReader cs g) = CmdReader cs ((fmap . fmap) f . g)

-- | A @Parser a@ is an option parser returning a value of type 'a'.
data Parser a where
  NilP :: Maybe a -> Parser a
  OptP :: Option a -> Parser a
  MultP :: Parser (a -> b) -> Parser a -> Parser b
  AltP :: Parser a -> Parser a -> Parser a
  BindP :: Parser a -> (a -> Parser b) -> Parser b

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

newtype ParserFailure = ParserFailure
  { execFailure :: String -> (String, ExitCode) }

instance Show ParserFailure where
  showsPrec p (ParserFailure f)
    = showParen (p > 10)
    $ showString "ParserFailure "
    . showsPrec 11 (f "<program>")

-- | Result of 'execParserPure'.
data ParserResult a
  = Success a
  | Failure ParserFailure
  | CompletionInvoked CompletionResult
  deriving Show

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

optVisibility :: Option a -> OptVisibility
optVisibility = propVisibility . optProps

optHelp :: Option a -> Chunk Doc
optHelp  = propHelp . optProps

optMetaVar :: Option a -> String
optMetaVar = propMetaVar . optProps

optShowDefault :: Option a -> Maybe String
optShowDefault = propShowDefault . optProps
