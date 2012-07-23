{-# LANGUAGE DeriveFunctor, EmptyDataDecls #-}
module Options.Applicative.Builder (
  -- * Parser builders
  --
  -- | This module contains utility functions and combinators to create parsers
  -- for individual options.
  --
  -- Each parser builder takes an option modifier. A modifier can be created by
  -- composing the basic modifiers provided by this module using the 'Monoid'
  -- operations 'mempty' and 'mappend', or their aliases 'idm' and '&'.
  --
  -- For example:
  --
  -- > out = strOption
  -- >     ( long "output"
  -- >     & short 'o'
  -- >     & metavar "FILENAME" )
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
  command,
  idm,
  (&),

  -- * Readers
  --
  -- | A collection of basic 'Option' readers.
  auto,
  str,
  disabled,

  -- * Internals
  Mod,
  HasName,
  OptionFields,
  FlagFields,
  CommandFields,

  -- * Builder for 'ParserInfo'
  InfoMod,
  fullDesc,
  header,
  progDesc,
  footer,
  failureCode,
  info,

  -- * Builder for 'ParserPrefs'
  PrefsMod,
  multiSuffix,
  prefs
  ) where

import Control.Applicative
import Control.Monad
import Data.Lens.Common
import Data.Maybe
import Data.Monoid

import Options.Applicative.Common
import Options.Applicative.Types

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

data ArgumentFields a
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

data Mod f a = Mod (f a -> f a)
                   (Maybe a)
                   (OptProperties -> OptProperties)

optionMod :: (OptProperties -> OptProperties) -> Mod f a
optionMod = Mod id Nothing

fieldMod :: (f a -> f a) -> Mod f a
fieldMod f = Mod f Nothing id

instance Monoid (Mod f a) where
  mempty = Mod id Nothing id
  Mod f1 d1 g1 `mappend` Mod f2 d2 g2
    = Mod (f2 . f1) (d2 `mplus` d1) (g2 . g1)

-- readers --

-- | 'Option' reader based on the 'Read' type class.
auto :: Read a => String -> Maybe a
auto arg = case reads arg of
  [(r, "")] -> Just r
  _         -> Nothing

-- | String 'Option' reader.
str :: String -> Maybe String
str = Just

-- | Null 'Option' reader. All arguments will fail validation.
disabled :: String -> Maybe a
disabled = const Nothing

-- modifiers --

-- | Specify a short name for an option.
short :: HasName f => Char -> Mod f a
short = fieldMod . name . OptShort

-- | Specify a long name for an option.
long :: HasName f => String -> Mod f a
long = fieldMod . name . OptLong

---- | Specify a default value for an option.
value :: a -> Mod f a
value x = Mod id (Just x) id

-- | Specify the help text for an option.
help :: String -> Mod f a
help = optionMod . setL propHelp

-- | Specify the 'Option' reader.
reader :: (String -> Maybe a) -> Mod OptionFields a
reader = fieldMod . setL optReader

-- | Specify the metavariable.
metavar :: String -> Mod f a
metavar = optionMod . setL propMetaVar

-- | Hide this option from the brief description.
hidden :: Mod f a
hidden = optionMod $ propVisibility ^%= min Hidden

-- | Hide this option from the help text
internal :: Mod f a
internal = optionMod $ propVisibility ^= Internal

-- | Add a command to a subparser option.
command :: String -> ParserInfo a -> Mod CommandFields a
command cmd pinfo = fieldMod $ cmdCommands^%=((cmd, pinfo):)

-- parsers --

-- | Base default properties.
baseProps :: OptProperties
baseProps = OptProperties
  { _propMetaVar = ""
  , _propVisibility = Visible
  , _propHelp = "" }

mkOption :: Maybe a -> Option a -> Parser a
mkOption def opt = liftOpt opt <|> maybe empty pure def

-- | Builder for a command parser. The 'command' modifier can be used to
-- specify individual commands.
subparser :: Mod CommandFields a -> Parser a
subparser m = mkOption def $ Option rdr (g baseProps)
  where
    Mod f def g = m & metavar "COMMAND"
    CommandFields cmds = f (CommandFields [])
    rdr = CmdReader (map fst cmds) (`lookup` cmds)

-- | Builder for an argument parser.
argument :: (String -> Maybe a) -> Mod ArgumentFields a -> Parser a
argument p (Mod _ def g) = mkOption def $ Option (ArgReader p) (g baseProps)

-- | Builder for an argument list parser. All arguments are collected and
-- returned as a list.
--
-- Note that arguments starting with @'-'@ are ignored.
--
-- This parser accepts a special argument: @--@. When a @--@ is found on the
-- command line, all following arguments are included in the result, even if
-- they start with @'-'@.
arguments :: (String -> Maybe a) -> Mod ArgumentFields [a] -> Parser [a]
arguments p m = args1 <|> pure (fromMaybe [] def)
  where
    Mod _ def g = m

    p' ('-':_) = Nothing
    p' s = p s

    args1 = ((Just <$> arg') <|> (ddash *> pure Nothing)) `BindP` \x -> case x of
      Nothing -> many arg
      Just a -> fmap (a:) args
    args = args1 <|> pure []

    arg' = argument p' (optionMod g)
    arg = argument p (optionMod g)

    ddash = argument (guard . (== "--")) internal

-- | Builder for a flag parser.
--
-- A flag that switches from a \"default value\" to an \"active value\" when
-- encountered. For a simple boolean value, use `switch` instead.
flag :: a                         -- ^ default value
     -> a                         -- ^ active value
     -> Mod FlagFields a          -- ^ option modifier
     -> Parser a
flag defv actv m = flag' actv m <|> pure defv

-- | Builder for a flag parser without a default value.
--
-- Same as 'flag', but with no default value. In particular, this flag will
-- never parse successfully by itself.
--
-- It still makes sense to use it as part of a composite parser. For example
--
-- > length <$> many (flag' () (short 't'))
--
-- is a parser that counts the number of "-t" arguments on the command line.
flag' :: a                         -- ^ active value
      -> Mod FlagFields a          -- ^ option modifier
      -> Parser a
flag' actv (Mod f def g) = mkOption def $ Option rdr (g baseProps)
  where
    rdr = let FlagFields ns actv' = f (FlagFields [] actv)
          in FlagReader ns actv'

-- | Builder for a boolean flag.
--
-- > switch = flag False True
switch :: Mod FlagFields Bool -> Parser Bool
switch = flag False True

-- | Builder for an option with a null reader. A non-trivial reader can be
-- added using the 'reader' modifier.
nullOption :: Mod OptionFields a -> Parser a
nullOption (Mod f def g) = mkOption def $ Option rdr (g baseProps)
  where
    rdr = let fields = f (OptionFields [] disabled)
          in OptReader (fields^.optNames) (fields^.optReader)

-- | Builder for an option taking a 'String' argument.
strOption :: Mod OptionFields String -> Parser String
strOption m = nullOption $ m & reader str

-- | Builder for an option using the 'auto' reader.
option :: Read a => Mod OptionFields a -> Parser a
option m = nullOption $ m & reader auto

-- | Modifier for 'ParserInfo'.
newtype InfoMod a = InfoMod
  { applyInfoMod :: ParserInfo a -> ParserInfo a }

instance Monoid (InfoMod a) where
  mempty = InfoMod id
  mappend m1 m2 = InfoMod $ applyInfoMod m2 . applyInfoMod m1

-- | Specify a full description for this parser.
fullDesc :: InfoMod a
fullDesc = InfoMod $ infoFullDesc^=True

-- | Specify a header for this parser.
header :: String -> InfoMod a
header s = InfoMod $ infoHeader^=s

-- | Specify a footer for this parser.
footer :: String -> InfoMod a
footer s = InfoMod $ infoFooter^=s

-- | Specify a short program description.
progDesc :: String -> InfoMod a
progDesc s = InfoMod $ infoProgDesc^=s

-- | Specify an exit code if a parse error occurs.
failureCode :: Int -> InfoMod a
failureCode n = InfoMod $ infoFailureCode^=n

-- | Create a 'ParserInfo' given a 'Parser' and a modifier.
info :: Parser a -> InfoMod a -> ParserInfo a
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

newtype PrefsMod = PrefsMod
  { applyPrefsMod :: ParserPrefs -> ParserPrefs }

instance Monoid PrefsMod where
  mempty = PrefsMod id
  mappend m1 m2 = PrefsMod $ applyPrefsMod m2 . applyPrefsMod m1

multiSuffix :: String -> PrefsMod
multiSuffix s = PrefsMod $ prefMultiSuffix ^= s

prefs :: PrefsMod -> ParserPrefs
prefs m = applyPrefsMod m base
  where
    base = ParserPrefs
      { _prefMultiSuffix = "" }

-- convenience shortcuts

idm :: Monoid m => m
idm = mempty

(&) :: Monoid m => m -> m -> m
(&) = mappend
