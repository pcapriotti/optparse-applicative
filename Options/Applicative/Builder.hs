module Options.Applicative.Builder where

import Control.Applicative
import Data.Lens.Common
import Options.Applicative

-- lenses --

mainL :: Lens (OptionGroup r a) (Option r)
mainL = lens optMain $ \m opts -> opts { optMain = m }

aliasesL :: Lens (OptionGroup r a) [Option r]
aliasesL = lens optAliases $ \as opts -> opts { optAliases = as }

defaultL :: Lens (OptionGroup r a) (Maybe a)
defaultL = lens optDefault $ \x opts -> opts { optDefault = x }

helpL :: Lens (OptionGroup r a) String
helpL = lens optHelp $ \h opts -> opts { optHelp = h }

setName :: OptName -> Option a -> Option a
setName name (Option _ p) = Option name p
setName name (Flag _ x) = Flag name x
setName _ opt = opt

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

long :: String -> Option r -> Option r
long = setName . OptLong

short :: Char -> Option r -> Option r
short = setName . OptShort

value :: a -> OptionGroup r a -> OptionGroup r a
value r = defaultL ^= Just r

help :: String -> OptionGroup r a -> OptionGroup r a
help htext = helpL ^= htext

reader :: (String -> Maybe a) -> Option a -> Option a
reader p (Option name _) = Option name p
reader _ opt = opt

flag :: a -> Option a -> Option a
flag x (Option name _) = Flag name x
flag x (Flag name _) = Flag name x
flag _ opt = opt

alt :: OptionGroup r a -> Option r -> Option r
alt opts _ = optMain opts

this :: (Option r -> Option r)
     -> OptionGroup r a
     -> OptionGroup r a
this = modL mainL

alias :: (Option r -> Option r)
      -> OptionGroup r a
      -> OptionGroup r a
alias f opts = modL aliasesL (opt:) opts
  where opt = f (opts ^. mainL)

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
  , optAliases = []
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

nullOption :: String -> (OptionGroup a a -> OptionGroup a b) -> Parser b
nullOption lname = baseParser $ Option (OptLong lname) disabled

strOption :: String -> (OptionGroup String String -> OptionGroup String a) -> Parser a
strOption lname f = nullOption lname (f . this (reader str))

option :: Read a => String -> (OptionGroup a a -> OptionGroup a b) -> Parser b
option lname f = nullOption lname (f . this (reader auto))
