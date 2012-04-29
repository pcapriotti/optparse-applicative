module Options.Applicative.Builder where

import Control.Applicative
import Data.Lens.Common
import Options.Applicative

-- lenses --

mainL :: Lens (Option r a) (OptReader r)
mainL = lens optMain $ \m opt -> opt { optMain = m }

defaultL :: Lens (Option r a) (Maybe a)
defaultL = lens optDefault $ \x opt -> opt { optDefault = x }

helpL :: Lens (Option r a) String
helpL = lens optHelp $ \h opt -> opt { optHelp = h }

metaVarL :: Lens (Option r a) String
metaVarL = lens optMetaVar $ \mv opt -> opt { optMetaVar = mv }

showL :: Lens (Option r a) Bool
showL = lens optShow $ \s opt -> opt { optShow = s }

addName :: OptName -> OptReader a -> OptReader a
addName name (OptReader names p) = OptReader (name : names) p
addName name (FlagReader names x) = FlagReader (name : names) x
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

long :: String -> Option r a -> Option r a
long = modL mainL . addName . OptLong

short :: Char -> Option r a -> Option r a
short = modL mainL . addName . OptShort

value :: a -> Option r a -> Option r a
value r = defaultL ^= Just r

help :: String -> Option r a -> Option r a
help htext = helpL ^= htext

reader :: (String -> Maybe r) -> Option r a -> Option r a
reader p = modL mainL $ \opt -> case opt of
  OptReader names _ -> OptReader names p
  _ -> opt

metavar :: String -> Option r a -> Option r a
metavar = setL metaVarL

hide :: Option r a -> Option r a
hide = showL ^= False

multi :: Option r a -> Option r [a]
multi opt = mkOptGroup []
  where
    mkOptGroup xs = opt
      { optDefault = Just xs
      , optCont = mkCont xs }
    mkCont xs r = do
      p' <- optCont opt r
      x <- evalParser p'
      return $ liftOpt (mkOptGroup (x:xs))

baseOpts :: OptReader a -> Option a a
baseOpts opt = Option
  { optMain = opt
  , optMetaVar = ""
  , optShow = True
  , optCont = Just . pure
  , optHelp = ""
  , optDefault = Nothing }

baseParser :: OptReader a -> (Option a a -> Option a b) -> Parser b
baseParser opt f = liftOpt $ f (baseOpts opt)

command :: (String -> Maybe (Parser a)) -> (Option a a -> Option a b) -> Parser b
command = baseParser . CmdReader

argument :: (String -> Maybe a) -> (Option a a -> Option a b) -> Parser b
argument = baseParser . ArgReader

arguments :: (String -> Maybe a) -> (Option a [a] -> Option a b) -> Parser b
arguments p f = argument p (f . multi)

flag :: a -> (Option a a -> Option a b) -> Parser b
flag = baseParser . FlagReader []

nullOption :: (Option a a -> Option a b) -> Parser b
nullOption = baseParser $ OptReader [] disabled

strOption :: (Option String String -> Option String a) -> Parser a
strOption f = nullOption $ f . reader str

option :: Read a => (Option a a -> Option a b) -> Parser b
option f = nullOption $ f . reader auto
