{-# LANGUAGE GADTs, DeriveFunctor, TemplateHaskell #-}
module Options.Applicative.Types (
  ParserInfo(..),
  info,

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
import Data.Lens.Template

-- | A full description for a runnable 'Parser' for a program.
data ParserInfo a = ParserInfo
  { infoParser :: Parser a            -- ^ the option parser for the program
  , infoFullDesc :: Bool              -- ^ whether the help text should contain full documentation
  , infoProgDesc :: String            -- ^ brief parser description
  , infoHeader :: String              -- ^ header of the full parser description
  , infoFooter :: String              -- ^ footer of the full parser description
  , infoFailureCode :: Int            -- ^ exit code for a parser failure
  } deriving Functor

-- | Create a default 'ParserInfo' for a given 'Parser'.
info :: Parser a -> ParserInfo a
info parser = ParserInfo
  { infoParser = parser
  , infoFullDesc = True
  , infoHeader = ""
  , infoProgDesc = ""
  , infoFooter = ""
  , infoFailureCode = 1 }

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

$( makeLenses [''Option] )
