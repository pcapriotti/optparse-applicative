{-# LANGUAGE GADTs, DeriveFunctor #-}
module Options.Applicative.Types (
  ParserInfo(..),
  ParserDesc(..),
  Context(..),
  P,

  infoParser,
  infoDesc,
  infoFullDesc,
  infoProgDesc,
  infoHeader,
  infoFooter,
  infoFailureCode,

  descFull,
  descProg,
  descHeader,
  descFooter,
  descFailureCode,

  Option(..),
  OptName(..),
  OptReader(..),
  OptVisibility(..),
  Parser(..),
  ParserFailure(..),

  optMain,
  optDefault,
  optVisibility,
  optHelp,
  optMetaVar,
  optCont
  ) where

import Control.Applicative
import Control.Category
import Control.Monad
import Control.Monad.Trans.Error
import Control.Monad.Trans.Writer
import Data.Lens.Common
import Data.Monoid
import Prelude hiding ((.), id)
import System.Exit

-- | A full description for a runnable 'Parser' for a program.
data ParserInfo a = ParserInfo
  { _infoParser :: Parser a            -- ^ the option parser for the program
  , _infoDesc :: ParserDesc            -- ^ description of the parser
  } deriving Functor

-- | Attributes that can be associated to a 'Parser'.
data ParserDesc = ParserDesc
  { _descFull:: Bool              -- ^ whether the help text should contain full documentation
  , _descProg:: String            -- ^ brief parser description
  , _descHeader :: String         -- ^ header of the full parser description
  , _descFooter :: String         -- ^ footer of the full parser description
  , _descFailureCode :: Int       -- ^ exit code for a parser failure
  }

data Context where
  Context :: Maybe String -> ParserInfo a -> Context
  NullContext :: Context

instance Monoid Context where
  mempty = NullContext
  mappend _ c@(Context _ _) = c
  mappend c _ = c

type P = ErrorT String (Writer Context)

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
data Option r a = Option
  { _optMain :: OptReader r               -- ^ reader for this option
  , _optDefault :: Maybe a                -- ^ default value
  , _optVisibility :: OptVisibility       -- ^ whether this flag is shown is the brief description
  , _optHelp :: String                    -- ^ help text for this option
  , _optMetaVar :: String                 -- ^ metavariable for this option
  , _optCont :: r -> P (Parser a) }       -- ^ option continuation
  deriving Functor

-- | An 'OptReader' defines whether an option matches an command line argument.
data OptReader a
  = OptReader [OptName] (String -> Maybe a)             -- ^ option reader
  | FlagReader [OptName] !a                             -- ^ flag reader
  | ArgReader (String -> Maybe a)                       -- ^ argument reader
  | CmdReader [String] (String -> Maybe (ParserInfo a)) -- ^ command reader
  deriving Functor

-- | A @Parser a@ is an option parser returning a value of type 'a'.
data Parser a where
  NilP :: a -> Parser a
  OptP :: Option r a -> Parser a
  MultP :: Parser (a -> b)
        -> Parser a
        -> Parser b

instance Functor Parser where
  fmap f (NilP x) = NilP (f x)
  fmap f (OptP opt) = OptP (fmap f opt)
  fmap f (MultP p1 p2) = MultP (fmap (f.) p1) p2

instance Applicative Parser where
  pure = NilP
  NilP f <*> p = fmap f p
  p <*> NilP x = fmap ($ x) p
  p1 <*> p2 = MultP p1 p2

-- | Result after a parse error.
data ParserFailure = ParserFailure
  { errMessage :: String -> String -- ^ Function which takes the program name
                                   -- as input and returns an error message
  , errExitCode :: ExitCode        -- ^ Exit code to use for this error
  }

instance Error ParserFailure where
  strMsg msg = ParserFailure
    { errMessage = \_ -> msg
    , errExitCode = ExitFailure 1 }

-- lenses

optMain :: Lens (Option r a) (OptReader r)
optMain = lens _optMain $ \x o -> o { _optMain = x }

optDefault :: Lens (Option r a) (Maybe a)
optDefault = lens _optDefault $ \x o -> o { _optDefault = x }

optVisibility :: Lens (Option r a) OptVisibility
optVisibility = lens _optVisibility $ \x o -> o { _optVisibility = x }

optHelp :: Lens (Option r a) String
optHelp = lens _optHelp $ \x o -> o { _optHelp = x }

optMetaVar :: Lens (Option r a) String
optMetaVar = lens _optMetaVar $ \x o -> o { _optMetaVar = x }

optCont :: Lens (Option r a) (r -> P (Parser a))
optCont = lens _optCont $ \x o -> o { _optCont = x }

descFull :: Lens ParserDesc Bool
descFull = lens _descFull $ \x p -> p { _descFull = x }

descProg :: Lens ParserDesc String
descProg = lens _descProg $ \x p -> p { _descProg = x }

descHeader :: Lens ParserDesc String
descHeader = lens _descHeader $ \x p -> p { _descHeader = x }

descFooter :: Lens ParserDesc String
descFooter = lens _descFooter $ \x p -> p { _descFooter = x }

descFailureCode :: Lens ParserDesc Int
descFailureCode = lens _descFailureCode $ \x p -> p { _descFailureCode = x }

infoParser :: Lens (ParserInfo a) (Parser a)
infoParser = lens _infoParser $ \x p -> p { _infoParser = x }

infoDesc :: Lens (ParserInfo a) ParserDesc
infoDesc = lens _infoDesc $ \x p -> p { _infoDesc = x }

infoFullDesc :: Lens (ParserInfo a) Bool
infoFullDesc = descFull . infoDesc

infoProgDesc :: Lens (ParserInfo a) String
infoProgDesc = descProg . infoDesc

infoHeader :: Lens (ParserInfo a) String
infoHeader = descHeader . infoDesc

infoFooter :: Lens (ParserInfo a) String
infoFooter = descFooter . infoDesc

infoFailureCode :: Lens (ParserInfo a) Int
infoFailureCode = descFailureCode . infoDesc
