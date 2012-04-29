module Options.Applicative.Builder where

import Control.Applicative
import Data.Lens.Common
import Options.Applicative

-- lenses --

mainL :: Lens (OptionGroup r a) (Option r)
mainL = lens optMain $ \m opts -> opts { optMain = m }

defaultL :: Lens (OptionGroup r a) (Maybe a)
defaultL = lens optDefault $ \x opts -> opts { optDefault = x }

helpL :: Lens (OptionGroup r a) String
helpL = lens optHelp $ \h opts -> opts { optHelp = h }

addName :: OptName -> Option a -> Option a
addName name (Option names p) = Option (name : names) p
addName name (Flag names x) = Flag (name : names) x
addName _ opt = opt

-- readers --

auto :: Read a => String -> Maybe a
auto arg = case reads arg of
  [(r, "")] -> Just r
  _         -> Nothing

str :: String -> Maybe String
str = Just

disabled :: String -> Maybe a
disabled = const Nothing

-- combinators --

long :: String -> OptionGroup r a -> OptionGroup r a
long = modL mainL . addName . OptLong

short :: Char -> OptionGroup r a -> OptionGroup r a
short = modL mainL . addName . OptShort

value :: a -> OptionGroup r a -> OptionGroup r a
value r = defaultL ^= Just r

help :: String -> OptionGroup r a -> OptionGroup r a
help htext = helpL ^= htext

reader :: (String -> Maybe r) -> OptionGroup r a -> OptionGroup r a
reader p = modL mainL $ \opt -> case opt of
  Option names _ -> Option names p
  _ -> opt

multi :: OptionGroup r a -> OptionGroup r [a]
multi opts = mkOptGroup []
  where
    mkOptGroup xs = opts
      { optDefault = Just xs
      , optCont = mkCont xs }
    mkCont xs r = do
      p' <- optCont opts r
      x <- evalParser p'
      return $ liftOpt (mkOptGroup (x:xs))

baseOpts :: Option a -> OptionGroup a a
baseOpts opt = OptionGroup
  { optMain = opt
  , optCont = Just . pure
  , optHelp = ""
  , optDefault = Nothing }

baseParser :: Option a -> (OptionGroup a a -> OptionGroup a b) -> Parser b
baseParser opt f = liftOpt $ f (baseOpts opt)

command :: (String -> Maybe (Parser a)) -> (OptionGroup a a -> OptionGroup a b) -> Parser b
command = baseParser . Command

argument :: (String -> Maybe a) -> (OptionGroup a a -> OptionGroup a b) -> Parser b
argument = baseParser . Argument

arguments :: (String -> Maybe a) -> (OptionGroup a [a] -> OptionGroup a b) -> Parser b
arguments p f = argument p (f . multi)

flag :: a -> (OptionGroup a a -> OptionGroup a b) -> Parser b
flag = baseParser . Flag []

nullOption :: (OptionGroup a a -> OptionGroup a b) -> Parser b
nullOption = baseParser $ Option [] disabled

strOption :: (OptionGroup String String -> OptionGroup String a) -> Parser a
strOption f = nullOption $ f . reader str

option :: Read a => (OptionGroup a a -> OptionGroup a b) -> Parser b
option f = nullOption $ f . reader auto
