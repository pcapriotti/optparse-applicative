{-# LANGUAGE GADTs, DeriveFunctor #-}
module Options.Applicative.Types (
  ParserInfo(..),
  ParserPrefs(..),
  Context(..),
  P,

  Option(..),
  OptName(..),
  OptReader(..),
  OptProperties(..),
  OptVisibility(..),
  Parser(..),
  ParserFailure(..),
  OptHelpInfo(..),

  optVisibility,
  optMetaVar,
  optHelp
  ) where

import Control.Applicative
import Control.Category
import Control.Monad
import Control.Monad.Trans.Error
import Control.Monad.Trans.Writer
import Data.Monoid
import Prelude hiding ((.), id)
import System.Exit

-- | A full description for a runnable 'Parser' for a program.
data ParserInfo a = ParserInfo
  { infoParser :: Parser a            -- ^ the option parser for the program
  , infoFullDesc :: Bool              -- ^ whether the help text should contain full documentation
  , infoProgDesc :: String            -- ^ brief parser description
  , infoHeader :: String              -- ^ header of the full parser description
  , infoFooter :: String              -- ^ footer of the full parser description
  , infoFailureCode :: Int            -- ^ exit code for a parser failure
  } deriving Functor

-- | Global preferences for a top-level 'Parser'.
data ParserPrefs = ParserPrefs
  { prefMultiSuffix :: String    -- ^ metavar suffix for multiple options
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
data OptProperties = OptProperties
  { propVisibility :: OptVisibility       -- ^ whether this flag is shown is the brief description
  , propHelp :: String                    -- ^ help text for this option
  , propMetaVar :: String                 -- ^ metavariable for this option
  }

-- | A single option of a parser.
data Option a = Option
  { optMain :: OptReader a               -- ^ reader for this option
  , optProps :: OptProperties            -- ^ properties of this option
  } deriving Functor

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

data OptHelpInfo = OptHelpInfo
  { hinfoMulti :: Bool
  , hinfoDefault :: Bool }

optVisibility :: Option a -> OptVisibility
optVisibility = propVisibility . optProps

optHelp :: Option a -> String
optHelp  = propHelp . optProps

optMetaVar :: Option a -> String
optMetaVar = propMetaVar . optProps
