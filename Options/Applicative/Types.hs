{-# LANGUAGE GADTs, DeriveFunctor #-}
module Options.Applicative.Types (
  ParserInfo(..),
  ParserDesc(..),

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
import Control.Category
import Control.Monad
import Data.Lens.Common
import Prelude hiding ((.), id)

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
