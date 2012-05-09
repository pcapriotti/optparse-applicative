{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
module Options.Applicative.Builder (
  -- * Readers
  auto,
  str,
  disabled,
  -- * Combinators
  short,
  long,
  help,
  value,
  metavar,
  reader,
  hide,
  multi,
  command,
  idm,
  (&),
  -- * Parsers
  subparser,
  argument,
  arguments,
  flag,
  nullOption,
  strOption,
  option
  ) where

import Control.Applicative
import Control.Category
import Data.Lens.Common
import Data.Lens.Template

import Options.Applicative
import Options.Applicative.Types

import Prelude hiding (id, (.))

data OptionFields a = OptionFields
  { _optNames :: [OptName]
  , _optReader :: String -> Maybe a }

data FlagFields a = FlagFields
  { _flagNames :: [OptName] }

data CommandFields a = CommandFields
  { _cmdCommands :: [(String, ParserInfo a)] }

$( makeLenses [ ''OptionFields
              , ''FlagFields
              , ''CommandFields ] )

class HasName f where
  name :: OptName -> f a -> f a

instance HasName OptionFields where
  name n = modL optNames (n:)

instance HasName FlagFields where
  name n = modL flagNames (n:)

-- mod --

data Mod f r a b = Mod (f r -> f r) (Option r a -> Option r b)

optionMod :: (Option r a -> Option r b) -> Mod f r a b
optionMod = Mod id

fieldMod :: (f r -> f r) -> Mod f r a a
fieldMod f = Mod f id

instance Category (Mod f r) where
  id = Mod id id
  Mod f1 g1 . Mod f2 g2 = Mod (f1 . f2) (g1 . g2)

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

short :: HasName f => Char -> Mod f r a a
short = fieldMod . name . OptShort

long :: HasName f => String -> Mod f r a a
long = fieldMod . name . OptLong

value :: a -> Mod f r a a
value = optionMod . setL optDefault . Just

help :: String -> Mod f r a a
help = optionMod . setL optHelp

reader :: (String -> Maybe r) -> Mod OptionFields r a a
reader = fieldMod . setL optReader

metavar :: String -> Mod f r a a
metavar = optionMod . setL optMetaVar

hide :: Mod f r a a
hide = optionMod $ optShow^=False

multi :: Mod f r a [a]
multi = optionMod f
  where
    f opt = mkOptGroup []
      where
        mkOptGroup xs = opt
          { _optDefault = Just xs
          , _optCont = mkCont xs }
        mkCont xs r = do
          p' <- getL optCont opt r
          x <- evalParser p'
          return $ liftOpt (mkOptGroup (x:xs))

command :: String -> ParserInfo r -> Mod CommandFields r a a
command cmd pinfo = fieldMod $ cmdCommands^%=((cmd, pinfo):)

-- parsers --

baseOpts :: OptReader a -> Option a a
baseOpts opt = Option
  { _optMain = opt
  , _optMetaVar = ""
  , _optShow = True
  , _optCont = Just . pure
  , _optHelp = ""
  , _optDefault = Nothing }

subparser :: Mod CommandFields a a b -> Parser b
subparser m = liftOpt . g . baseOpts $ opt
  where
    Mod f g = m . metavar "COMMAND"
    CommandFields cmds = f (CommandFields [])
    opt = CmdReader (map fst cmds) (`lookup` cmds)

argument :: (String -> Maybe a) -> Mod f a a b -> Parser b
argument p (Mod _ g) = liftOpt . g . baseOpts $ ArgReader p

arguments :: (String -> Maybe a) -> Mod f a [a] b -> Parser b
arguments p m = argument p (m . multi)

flag :: a -> Mod FlagFields a a b -> Parser b
flag x (Mod f g) = liftOpt . g . baseOpts $ rdr
  where
    rdr = let fields = f (FlagFields [])
          in FlagReader (fields^.flagNames) x

nullOption :: Mod OptionFields a a b -> Parser b
nullOption (Mod f g) = liftOpt . g . baseOpts $ rdr
  where
    rdr = let fields = f (OptionFields [] disabled)
          in OptReader (fields^.optNames) (fields^.optReader)

strOption :: Mod OptionFields String String a -> Parser a
strOption m = nullOption $ m . reader str

option :: Read a => Mod OptionFields a a b -> Parser b
option m = nullOption $ m . reader auto

idm :: Mod f r a a
idm = id

(&) :: Mod f r a b -> Mod f r b c -> Mod f r a c
(&) = flip (.)
