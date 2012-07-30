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
  showDefaultWith,
  showDefault,
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
import Data.Maybe
import Data.Monoid

import Options.Applicative.Common
import Options.Applicative.Types

data OptionFields a = OptionFields
  { optNames :: [OptName]
  , optReader :: String -> Maybe a }
  deriving Functor

data FlagFields a = FlagFields
  { flagNames :: [OptName]
  , flagActive :: a }
  deriving Functor

data CommandFields a = CommandFields
  { cmdCommands :: [(String, ParserInfo a)] }
  deriving Functor

data ArgumentFields a
  deriving Functor

class HasName f where
  name :: OptName -> f a -> f a

instance HasName OptionFields where
  name n fields = fields { optNames = n : optNames fields }

instance HasName FlagFields where
  name n fields = fields { flagNames = n : flagNames fields }

-- mod --

data DefaultProp a = DefaultProp
  (Maybe a)
  (Maybe (a -> String))

instance Monoid (DefaultProp a) where
  mempty = DefaultProp Nothing Nothing
  mappend (DefaultProp d1 s1) (DefaultProp d2 s2) =
    DefaultProp (d1 `mplus` d2) (s1 `mplus` s2)

data Mod f a = Mod (f a -> f a)
                   (DefaultProp a)
                   (OptProperties -> OptProperties)

optionMod :: (OptProperties -> OptProperties) -> Mod f a
optionMod = Mod id mempty

fieldMod :: (f a -> f a) -> Mod f a
fieldMod f = Mod f mempty id

instance Monoid (Mod f a) where
  mempty = Mod id mempty id
  Mod f1 d1 g1 `mappend` Mod f2 d2 g2
    = Mod (f2 . f1) (d2 <> d1) (g2 . g1)

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

-- | Specify a default value for an option.
value :: a -> Mod f a
value x = Mod id (DefaultProp (Just x) Nothing) id

-- | Specify a function to show the default value for an option.
showDefaultWith :: (a -> String) -> Mod f a
showDefaultWith s = Mod id (DefaultProp Nothing (Just s)) id

-- | Show the default value for this option using its 'Show' instance.
showDefault :: Show a => Mod f a
showDefault = showDefaultWith show

-- | Specify the help text for an option.
help :: String -> Mod f a
help s = optionMod $ \p -> p { propHelp = s }

-- | Specify the 'Option' reader.
reader :: (String -> Maybe a) -> Mod OptionFields a
reader f = fieldMod $ \p -> p { optReader = f }

-- | Specify the metavariable.
metavar :: String -> Mod f a
metavar var = optionMod $ \p -> p { propMetaVar = var }

-- | Hide this option from the brief description.
hidden :: Mod f a
hidden = optionMod $ \p ->
  p { propVisibility = min Hidden (propVisibility p) }

-- | Hide this option from the help text
internal :: Mod f a
internal = optionMod $ \p -> p { propVisibility = Internal }

-- | Add a command to a subparser option.
command :: String -> ParserInfo a -> Mod CommandFields a
command cmd pinfo = fieldMod $ \p ->
  p { cmdCommands = (cmd, pinfo) : cmdCommands p }

-- parsers --

-- | Base default properties.
baseProps :: OptProperties
baseProps = OptProperties
  { propMetaVar = ""
  , propVisibility = Visible
  , propHelp = ""
  , propShowDefault = Nothing }

mkParser :: DefaultProp a
         -> (OptProperties -> OptProperties)
         -> OptReader a
         -> Parser a
mkParser d@(DefaultProp def _) g rdr = liftOpt opt <|> maybe empty pure def
  where
    opt = mkOption d g rdr

mkOption :: DefaultProp a
         -> (OptProperties -> OptProperties)
         -> OptReader a
         -> Option a
mkOption d g rdr = Option rdr (mkProps d g)

mkProps :: DefaultProp a
        -> (OptProperties -> OptProperties)
        -> OptProperties
mkProps (DefaultProp def sdef) g = props
  where
    props = (g baseProps)
      { propShowDefault = sdef <*> def }


-- | Builder for a command parser. The 'command' modifier can be used to
-- specify individual commands.
subparser :: Mod CommandFields a -> Parser a
subparser m = mkParser d g rdr
  where
    Mod f d g = m & metavar "COMMAND"
    CommandFields cmds = f (CommandFields [])
    rdr = CmdReader (map fst cmds) (`lookup` cmds)

-- | Builder for an argument parser.
argument :: (String -> Maybe a) -> Mod ArgumentFields a -> Parser a
argument p (Mod _ d g) = mkParser d g (ArgReader p)

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
    Mod _ (DefaultProp def sdef) g = m
    show_def = sdef <*> def

    p' ('-':_) = Nothing
    p' s = p s

    props = mkProps mempty g
    props' = (mkProps mempty g) { propShowDefault = show_def }

    args1 = ((Just <$> arg') <|> (ddash *> pure Nothing)) `BindP` \x -> case x of
      Nothing -> many arg
      Just a -> fmap (a:) args
    args = args1 <|> pure []

    arg = liftOpt (Option (ArgReader p) props)
    arg' = liftOpt (Option (ArgReader p') props')

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
flag' actv (Mod f d g) = mkParser d g rdr
  where
    rdr = let fields = f (FlagFields [] actv)
          in FlagReader (flagNames fields)
                        (flagActive fields)

-- | Builder for a boolean flag.
--
-- > switch = flag False True
switch :: Mod FlagFields Bool -> Parser Bool
switch = flag False True

-- | Builder for an option with a null reader. A non-trivial reader can be
-- added using the 'reader' modifier.
nullOption :: Mod OptionFields a -> Parser a
nullOption (Mod f d g) = mkParser d g rdr
  where
    rdr = let fields = f (OptionFields [] disabled)
          in OptReader (optNames fields) (optReader fields)

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
fullDesc = InfoMod $ \i -> i { infoFullDesc = True }

-- | Specify a header for this parser.
header :: String -> InfoMod a
header s = InfoMod $ \i -> i { infoHeader = s }

-- | Specify a footer for this parser.
footer :: String -> InfoMod a
footer s = InfoMod $ \i -> i { infoFooter = s }

-- | Specify a short program description.
progDesc :: String -> InfoMod a
progDesc s = InfoMod $ \i -> i { infoProgDesc = s }

-- | Specify an exit code if a parse error occurs.
failureCode :: Int -> InfoMod a
failureCode n = InfoMod $ \i -> i { infoFailureCode = n }

-- | Create a 'ParserInfo' given a 'Parser' and a modifier.
info :: Parser a -> InfoMod a -> ParserInfo a
info parser m = applyInfoMod m base
  where
    base = ParserInfo
      { infoParser = parser
      , infoFullDesc = True
      , infoProgDesc = ""
      , infoHeader = ""
      , infoFooter = ""
      , infoFailureCode = 1 }

newtype PrefsMod = PrefsMod
  { applyPrefsMod :: ParserPrefs -> ParserPrefs }

instance Monoid PrefsMod where
  mempty = PrefsMod id
  mappend m1 m2 = PrefsMod $ applyPrefsMod m2 . applyPrefsMod m1

multiSuffix :: String -> PrefsMod
multiSuffix s = PrefsMod $ \p -> p { prefMultiSuffix = s }

prefs :: PrefsMod -> ParserPrefs
prefs m = applyPrefsMod m base
  where
    base = ParserPrefs
      { prefMultiSuffix = "" }

-- convenience shortcuts

--- | Trivial option modifier.
idm :: Monoid m => m
idm = mempty

--- | Compose modifiers.
(&) :: Monoid m => m -> m -> m
(&) = mappend
