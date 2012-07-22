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
  OptProperties(..),
  OptVisibility(..),
  Parser(..),
  ParserFailure(..),

  optMain,
  optDefault,
  optVisibility,
  optHelp,
  optMetaVar,
  propDefault,
  propVisibility,
  propHelp,
  propMetaVar,
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
data OptProperties a = OptProperties
  { _propDefault :: Maybe a                -- ^ default value
  , _propVisibility :: OptVisibility       -- ^ whether this flag is shown is the brief description
  , _propHelp :: String                    -- ^ help text for this option
  , _propMetaVar :: String                 -- ^ metavariable for this option
  } deriving Functor

-- | A single option of a parser.
data Option a = Option
  { _optMain :: OptReader a               -- ^ reader for this option
  , _optProps :: OptProperties a          -- ^ properties of this option
  }
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

instance Alternative Parser where
  empty = NilP Nothing
  (<|>) = AltP
  many p = some p <|> pure []
  some p = p `BindP` (\r -> (r:) <$> many p)

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

optMain :: Lens (Option a) (OptReader a)
optMain = lens _optMain $ \x o -> o { _optMain = x }

optProps :: Lens (Option a) (OptProperties a)
optProps = lens _optProps $ \x o -> o { _optProps = x }

propDefault :: Lens (OptProperties a) (Maybe a)
propDefault = lens _propDefault $ \x o -> o { _propDefault = x }

propVisibility :: Lens (OptProperties a) OptVisibility
propVisibility = lens _propVisibility $ \x o -> o { _propVisibility = x }

propHelp :: Lens (OptProperties a) String
propHelp = lens _propHelp $ \x o -> o { _propHelp = x }

propMetaVar :: Lens (OptProperties a) String
propMetaVar = lens _propMetaVar $ \x o -> o { _propMetaVar = x }

optDefault :: Lens (Option a) (Maybe a)
optDefault = propDefault . optProps

optVisibility :: Lens (Option a) OptVisibility
optVisibility = propVisibility . optProps

optHelp :: Lens (Option a) String
optHelp = propHelp . optProps

optMetaVar :: Lens (Option a) String
optMetaVar = propMetaVar . optProps

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
