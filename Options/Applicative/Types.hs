{-# LANGUAGE GADTs, DeriveFunctor #-}
module Options.Applicative.Types (
  ParserInfo(..),

  infoParser,
  infoFullDesc,
  infoProgDesc,
  infoHeader,
  infoFooter,
  infoFailureCode,

  Option(..),
  OptName(..),
  OptReader(..),
  Parser(..),
  P(..),

  optMain,
  optDefault,
  optShow,
  optHelp,
  optMetaVar,
  optCont
  ) where

import Control.Applicative
import Control.Monad
import Data.Lens.Common

-- | A full description for a runnable 'Parser' for a program.
data ParserInfo a = ParserInfo
  { _infoParser :: Parser a            -- ^ the option parser for the program
  , _infoFullDesc :: Bool              -- ^ whether the help text should contain full documentation
  , _infoProgDesc :: String            -- ^ brief parser description
  , _infoHeader :: String              -- ^ header of the full parser description
  , _infoFooter :: String              -- ^ footer of the full parser description
  , _infoFailureCode :: Int            -- ^ exit code for a parser failure
  } deriving Functor


data OptName = OptShort !Char
             | OptLong !String
  deriving (Eq, Ord)

-- | Specification for an individual parser option.
data Option r a = Option
  { _optMain :: OptReader r               -- ^ reader for this option
  , _optDefault :: Maybe a                -- ^ default value
  , _optShow :: Bool                      -- ^ whether this flag is shown is the brief description
  , _optHelp :: String                    -- ^ help text for this option
  , _optMetaVar :: String                 -- ^ metavariable for this option
  , _optCont :: r -> Maybe (Parser a) }   -- ^ option continuation
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
  ConsP :: Option r (a -> b)
        -> Parser a
        -> Parser b

instance Functor Parser where
  fmap f (NilP x) = NilP (f x)
  fmap f (ConsP opt p) = ConsP (fmap (f.) opt) p

instance Applicative Parser where
  pure = NilP
  NilP f <*> p = fmap f p
  ConsP opt p1 <*> p2 =
    ConsP (fmap uncurry opt) $ (,) <$> p1 <*> p2

data P a
  = ParseError
  | ParseResult a
  deriving Functor

instance Monad P where
  return = ParseResult
  ParseError >>= _ = ParseError
  ParseResult a >>= f = f a
  fail _ = ParseError

instance Applicative P where
  pure = return
  (<*>) = ap

-- lenses

optMain :: Lens (Option r a) (OptReader r)
optMain = lens _optMain $ \x o -> o { _optMain = x }

optDefault :: Lens (Option r a) (Maybe a)
optDefault = lens _optDefault $ \x o -> o { _optDefault = x }

optShow :: Lens (Option r a) Bool
optShow = lens _optShow $ \x o -> o { _optShow = x }

optHelp :: Lens (Option r a) String
optHelp = lens _optHelp $ \x o -> o { _optHelp = x }

optMetaVar :: Lens (Option r a) String
optMetaVar = lens _optMetaVar $ \x o -> o { _optMetaVar = x }

optCont :: Lens (Option r a) (r -> Maybe (Parser a))
optCont = lens _optCont $ \x o -> o { _optCont = x }

infoParser :: Lens (ParserInfo a) (Parser a)
infoParser = lens _infoParser $ \x p -> p { _infoParser = x }

infoFullDesc :: Lens (ParserInfo a) Bool
infoFullDesc = lens _infoFullDesc $ \x p -> p { _infoFullDesc = x }

infoProgDesc :: Lens (ParserInfo a) String
infoProgDesc = lens _infoProgDesc $ \x p -> p { _infoProgDesc = x }

infoHeader :: Lens (ParserInfo a) String
infoHeader = lens _infoHeader $ \x p -> p { _infoHeader = x }

infoFooter :: Lens (ParserInfo a) String
infoFooter = lens _infoFooter $ \x p -> p { _infoFooter = x }

infoFailureCode :: Lens (ParserInfo a) Int
infoFailureCode = lens _infoFailureCode $ \x p -> p { _infoFailureCode = x }
