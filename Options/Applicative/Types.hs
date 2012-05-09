{-# LANGUAGE GADTs, DeriveFunctor, TemplateHaskell #-}
module Options.Applicative.Types where

import Control.Applicative
import Control.Monad
import Data.Lens.Template

data ParserInfo a = ParserInfo
  { infoParser :: Parser a
  , infoFullDesc :: Bool
  , infoHeader :: String
  , infoProgDesc :: String
  , infoFooter :: String
  , infoFailureCode :: Int }
  deriving Functor

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

data Option r a = Option
  { _optMain :: OptReader r
  , _optDefault :: Maybe a
  , _optShow :: Bool
  , _optHelp :: String
  , _optMetaVar :: String
  , _optCont :: r -> Maybe (Parser a) }
  deriving Functor

data OptReader a
  = OptReader [OptName] (String -> Maybe a)
  | FlagReader [OptName] !a
  | ArgReader (String -> Maybe a)
  | CmdReader (String -> Maybe (ParserInfo a))
  deriving Functor

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
