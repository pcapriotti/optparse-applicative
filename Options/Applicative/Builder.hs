{-# LANGUAGE DeriveFunctor #-}
module Options.Applicative.Builder (
  -- * Parser builders
  --
  -- | This module contains utility functions and combinators to create parsers
  -- for individual options.
  --
  -- Each parser builder takes an option modifier, which can be specified by
  -- composing basic modifiers using '&' and 'idm' (which are just convenient
  -- synonyms for the 'Category' operations 'Control.Category.>>>' and
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
  flag',
  switch,
  nullOption,
  strOption,
  option,

  -- * Modifiers
  short,
  long,
  help,
  value,
  metavar,
  reader,
  hidden,
  internal,
  transform,
  command,
  idm,
  (&),

  -- * Readers
  --
  -- | A collection of basic Optioneaders.
  auto,
  str,
  disabled,

  -- * Internals
  Mod,
  HasName,
  OptionFields,
  FlagFields,
  CommandFields,

  -- * Builder for `ParserInfo`
  InfoMod,
  fullDesc,
  header,
  progDesc,
  footer,
  failureCode,
  info
  ) where

import Control.Applicative
import Control.Category
import Control.Monad
import Data.Functor.Identity
import Data.Lens.Common

import Options.Applicative.Common
import Options.Applicative.Types

import Prelude hiding (id, (.))

data OptionFields a = OptionFields
  { _optNames :: [OptName]
  , _optReader :: String -> Maybe a }
  deriving Functor

data FlagFields a = FlagFields
  { _flagNames :: [OptName]
  , _flagActive :: a }
  deriving Functor

data CommandFields a = CommandFields
  { _cmdCommands :: [(String, ParserInfo a)] }
  deriving Functor

optNames :: Lens (OptionFields a) [OptName]
optNames = lens _optNames $ \x o -> o { _optNames = x }

optReader :: Lens (OptionFields a) (String -> Maybe a)
optReader = lens _optReader $ \x o -> o { _optReader = x }

flagNames :: Lens (FlagFields a) [OptName]
flagNames = lens _flagNames $ \x o -> o { _flagNames = x }

cmdCommands :: Lens (CommandFields a) [(String, ParserInfo a)]
cmdCommands = lens _cmdCommands $ \x o -> o { _cmdCommands = x }

class HasName f where
  name :: OptName -> f a -> f a

instance HasName OptionFields where
  name n = modL optNames (n:)

instance HasName FlagFields where
  name n = modL flagNames (n:)

-- mod --

data Mod f a b = Mod (f a -> f b) (OptProperties a -> OptProperties b)

optionMod :: (OptProperties a -> OptProperties a) -> Mod f a a
optionMod = Mod id

fieldMod :: (f a -> f a) -> Mod f a a
fieldMod f = Mod f id

instance Category (Mod f) where
  id = Mod id id
  Mod f1 g1 . Mod f2 g2 = Mod (f1 . f2) (g1 . g2)

-- readers --

-- | Optioneader based on the 'Read' type class.
auto :: Read a => String -> Maybe a
auto arg = case reads arg of
  [(r, "")] -> Just r
  _         -> Nothing

-- | String Optioneader.
str :: String -> Maybe String
str = Just

-- | Null Optioneader. All arguments will fail validation.
disabled :: String -> Maybe a
disabled = const Nothing

-- modifiers --

-- | Specify a short name for an option.
short :: HasName f => Char -> Mod f a a
short = fieldMod . name . OptShort

-- | Specify a long name for an option.
long :: HasName f => String -> Mod f a a
long = fieldMod . name . OptLong

-- | Specify a default value for an option.
value :: a -> Mod f a a
value = optionMod . setL propDefault . Just

-- | Specify the help text for an option.
help :: String -> Mod f a a
help = optionMod . setL propHelp

-- | Specify the Optioneader.
reader :: (String -> Maybe a) -> Mod OptionFields a a
reader = fieldMod . setL optReader

-- | Specify the metavariable.
metavar :: String -> Mod f a a
metavar = optionMod . setL propMetaVar

-- | Hide this option from the brief description.
hidden :: Mod f a a
hidden = optionMod $ propVisibility ^%= min Hidden

-- | Hide this option from the help text
internal :: Mod f a a
internal = optionMod $ propVisibility ^= Internal

-- | Apply a transformation to the return value of this option.
--
-- This can be used, for example, to provide a default value for
-- a required option, like:
--
-- >strOption
-- >( transform Just
-- >& value Nothing )
transform :: Functor f => (a -> b) -> Mod f a b
transform f = Mod (fmap f) (fmap f)

-- | Add a command to a subparser option.
command :: String -> ParserInfo a -> Mod CommandFields a a
command cmd pinfo = fieldMod $ cmdCommands^%=((cmd, pinfo):)

-- parsers --

-- | Base default properties.
baseProps :: OptProperties a
baseProps = OptProperties
  { _propMetaVar = ""
  , _propVisibility = Visible
  , _propHelp = ""
  , _propDefault = Nothing }

-- | Builder for a command parser. The 'command' modifier can be used to
-- specify individual commands.
subparser :: Mod CommandFields a b -> Parser b
subparser m = liftOpt $ Option rdr (g baseProps)
  where
    Mod f g = m . metavar "COMMAND"
    CommandFields cmds = f (CommandFields [])
    rdr = CmdReader (map fst cmds) (`lookup` cmds)

-- | Builder for an argument parser.
argument :: (String -> Maybe a) -> Mod Identity a b -> Parser b
argument p (Mod f g) = liftOpt $ Option (ArgReader p') (g baseProps)
  where p' s = fmap (runIdentity . f . Identity) (p s)

-- | Builder for an argument list parser. All arguments are collected and
-- returned as a list.
arguments :: (String -> Maybe a) -> Mod Identity a b -> Parser [b]
arguments p m = many $ argument p m

-- | Builder for a flag parser.
--
-- A flag that switches from a \"default value\" to an \"active value\" when
-- encountered. For a simple boolean value, use `switch` instead.
flag :: a                         -- ^ default value
     -> a                         -- ^ active value
     -> Mod FlagFields a b        -- ^ option modifier
     -> Parser b
flag defv actv m = flag' actv (m . value defv)

-- | Builder for a flag parser without a default value.
--
-- Same as 'flag', but with no default value. In particular, this flag will
-- never parse successfully by itself.
--
-- It still makes sense to use it as part of a composite parser. For example
--
-- > length <$> multiP (flag' () (short 't'))
--
-- is a parser that counts the number of "-t" arguments on the command line.
flag' :: a                         -- ^ active value
      -> Mod FlagFields a b        -- ^ option modifier
      -> Parser b
flag' actv (Mod f g) = liftOpt $ Option rdr (g baseProps)
  where
    rdr = let FlagFields ns actv' = f (FlagFields [] actv)
          in FlagReader ns actv'

-- | Builder for a boolean flag.
--
-- > switch = flag False True
switch :: Mod FlagFields Bool a -> Parser a
switch = flag False True

-- | Builder for an option with a null reader. A non-trivial reader can be
-- added using the 'reader' modifier.
nullOption :: Mod OptionFields a b -> Parser b
nullOption (Mod f g) = liftOpt $ Option rdr (g baseProps)
  where
    rdr = let fields = f (OptionFields [] disabled)
          in OptReader (fields^.optNames) (fields^.optReader)

-- | Builder for an option taking a 'String' argument.
strOption :: Mod OptionFields String a -> Parser a
strOption m = nullOption $ m . reader str

-- | Builder for an option using the 'auto' reader.
option :: Read a => Mod OptionFields a b -> Parser b
option m = nullOption $ m . reader auto

-- | Modifier for 'ParserInfo'.
newtype InfoMod a b = InfoMod
  { applyInfoMod :: ParserInfo a -> ParserInfo b }

instance Category InfoMod where
  id = InfoMod id
  m1 . m2 = InfoMod $ applyInfoMod m1 . applyInfoMod m2

-- | Specify a full description for this parser.
fullDesc :: InfoMod a a
fullDesc = InfoMod $ infoFullDesc^=True

-- | Specify a header for this parser.
header :: String -> InfoMod a a
header s = InfoMod $ infoHeader^=s

-- | Specify a footer for this parser.
footer :: String -> InfoMod a a
footer s = InfoMod $ infoFooter^=s

-- | Specify a short program description.
progDesc :: String -> InfoMod a a
progDesc s = InfoMod $ infoProgDesc^=s

-- | Specify an exit code if a parse error occurs.
failureCode :: Int -> InfoMod a a
failureCode n = InfoMod $ infoFailureCode^=n

-- | Create a 'ParserInfo' given a 'Parser' and a modifier.
info :: Parser a -> InfoMod a a -> ParserInfo a
info parser m = applyInfoMod m base
  where
    base = ParserInfo
      { _infoParser = parser
      , _infoDesc = ParserDesc
        { _descFull = True
        , _descProg = ""
        , _descHeader = ""
        , _descFooter = ""
        , _descFailureCode = 1
        }
      }

-- | Trivial option modifier.
idm :: Category hom => hom a a
idm = id

-- | Compose modifiers.
(&) :: Category hom => hom a b -> hom b c -> hom a c
(&) = flip (.)
