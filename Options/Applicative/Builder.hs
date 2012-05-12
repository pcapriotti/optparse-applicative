{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
module Options.Applicative.Builder (
  -- * Parser builders
  --
  -- | This module contains utility functions and combinators to create parsers
  -- for individual options.
  --
  -- Each parser builder takes an option modifier, which can be specified by
  -- composing basic combinators using '&' and 'idm' (which are just
  -- specializations of the 'Category' operations 'Control.Category.>>>' and
  -- 'Control.Category.id').
  --
  -- For example:
  --
  --
  -- > out = strOption
  -- >     ( long "output"
  -- >     & short 'o'
  -- >     & metavar "FILENAME" )
  --
  --
  -- creates a parser for an option called \"output\".
  subparser,
  argument,
  arguments,
  flag,
  switch,
  nullOption,
  strOption,
  option,

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

  -- * Readers
  --
  -- | A collection of basic option readers.
  auto,
  str,
  disabled,

  -- * Internals
  Mod,
  HasName,
  OptionFields,
  FlagFields,
  CommandFields
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

-- | Option reader based on the 'Read' type class.
auto :: Read a => String -> Maybe a
auto arg = case reads arg of
  [(r, "")] -> Just r
  _         -> Nothing

-- | String option reader.
str :: String -> Maybe String
str = Just

-- | Null option reader. All arguments will fail validation.
disabled :: String -> Maybe a
disabled = const Nothing

-- combinators --

-- | Specify a short name for an option.
short :: HasName f => Char -> Mod f r a a
short = fieldMod . name . OptShort

-- | Specify a long name for an option.
long :: HasName f => String -> Mod f r a a
long = fieldMod . name . OptLong

-- | Specify a default value for an option.
value :: a -> Mod f r a a
value = optionMod . setL optDefault . Just

-- | Specify the help text for an option.
help :: String -> Mod f r a a
help = optionMod . setL optHelp

-- | Specify the option reader.
reader :: (String -> Maybe r) -> Mod OptionFields r a a
reader = fieldMod . setL optReader

-- | Specify the metavariable.
metavar :: String -> Mod f r a a
metavar = optionMod . setL optMetaVar

-- | Hide this option.
hide :: Mod f r a a
hide = optionMod $ optShow^=False

-- | Create a multi-valued option.
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

-- | Add a command to a subparser option.
command :: String -> ParserInfo r -> Mod CommandFields r a a
command cmd pinfo = fieldMod $ cmdCommands^%=((cmd, pinfo):)

-- parsers --

-- | Base default option.
baseOpts :: OptReader a -> Option a a
baseOpts opt = Option
  { _optMain = opt
  , _optMetaVar = ""
  , _optShow = True
  , _optCont = Just . pure
  , _optHelp = ""
  , _optDefault = Nothing }

-- | Builder for a command parser. The 'command' combinator can be used to
-- specify individual commands.
subparser :: Mod CommandFields a a b -> Parser b
subparser m = liftOpt . g . baseOpts $ opt
  where
    Mod f g = m . metavar "COMMAND"
    CommandFields cmds = f (CommandFields [])
    opt = CmdReader (map fst cmds) (`lookup` cmds)

-- | Builder for an argument parser.
argument :: (String -> Maybe a) -> Mod f a a b -> Parser b
argument p (Mod _ g) = liftOpt . g . baseOpts $ ArgReader p

-- | Builder for an argument list parser. All arguments are collected and
-- returned as a list.
arguments :: (String -> Maybe a) -> Mod f a [a] b -> Parser b
arguments p m = argument p (m . multi)

-- | Builder for a flag parser.
--
-- A flag that switches from a \"default value\" to an \"active value\" when
-- encountered. For a simple boolean value, use `switch` instead.
flag :: a                         -- ^ default value
     -> a                         -- ^ active value
     -> Mod FlagFields a a b      -- ^ option modifier
     -> Parser b
flag defv actv (Mod f g) = liftOpt . g . set_default . baseOpts $ rdr
  where
    rdr = let fields = f (FlagFields [])
          in FlagReader (fields^.flagNames) actv
    set_default = optDefault ^= Just defv

-- | Builder for a boolean flag.
--
-- > switch = flag False True
switch :: Mod FlagFields Bool Bool a -> Parser a
switch = flag False True

-- | Builder for an option with a null reader. A non-trivial reader can be
-- added using the 'reader' combinator.
nullOption :: Mod OptionFields a a b -> Parser b
nullOption (Mod f g) = liftOpt . g . baseOpts $ rdr
  where
    rdr = let fields = f (OptionFields [] disabled)
          in OptReader (fields^.optNames) (fields^.optReader)

-- | Builder for an option taking a 'String' argument.
strOption :: Mod OptionFields String String a -> Parser a
strOption m = nullOption $ m . reader str

-- | Builder for an option using the 'auto' reader.
option :: Read a => Mod OptionFields a a b -> Parser b
option m = nullOption $ m . reader auto

-- | Trivial option modifier.
idm :: Mod f r a a
idm = id

-- | Compose combinators.
(&) :: Mod f r a b -> Mod f r b c -> Mod f r a c
(&) = flip (.)
