{-# LANGUAGE CPP #-}
module Options.Applicative.Builder (
  -- * Parser builders
  --
  -- | This module contains utility functions and combinators to create parsers
  -- for individual options.
  --
  -- Each parser builder takes an option modifier. A modifier can be created by
  -- composing the basic modifiers provided by this module using the 'Monoid'
  -- operations 'mempty' and 'mappend', or their aliases 'idm' and '<>'.
  --
  -- For example:
  --
  -- > out = strOption
  -- >     ( long "output"
  -- >    <> short 'o'
  -- >    <> metavar "FILENAME" )
  --
  -- creates a parser for an option called \"output\".
  subparser,
  argument,
  arguments,
  arguments1,
  flag,
  flag',
  switch,
  nullOption,
  abortOption,
  infoOption,
  strOption,
  option,

  -- * Modifiers
  short,
  long,
  help,
  helpDoc,
  value,
  showDefaultWith,
  showDefault,
  metavar,
  reader,
  eitherReader,
  noArgError,
  ParseError(..),
  hidden,
  internal,
  command,
  completeWith,
  action,
  completer,
  idm,
  (&),
#if __GLASGOW_HASKELL__ > 702
  (<>),
#endif
  mappend,

  -- * Readers
  --
  -- | A collection of basic 'Option' readers.
  auto,
  str,
  disabled,
  readerAbort,
  readerError,

  -- * Builder for 'ParserInfo'
  InfoMod,
  fullDesc,
  briefDesc,
  header,
  headerDoc,
  footer,
  footerDoc,
  progDesc,
  progDescDoc,
  failureCode,
  noIntersperse,
  info,

  -- * Builder for 'ParserPrefs'
  PrefsMod,
  multiSuffix,
  disambiguate,
  showHelpOnError,
  noBacktrack,
  columns,
  prefs,

  -- * Types
  Mod,
  ReadM,
  OptionFields,
  FlagFields,
  ArgumentFields,
  CommandFields
  ) where

import Control.Applicative (pure, (<|>), many, some)
import Data.Monoid (Monoid (..)
#if __GLASGOW_HASKELL__ > 702
  , (<>)
#endif
  )

import Options.Applicative.Builder.Completer
import Options.Applicative.Builder.Internal
import Options.Applicative.Common
import Options.Applicative.Types
import Options.Applicative.Help.Pretty
import Options.Applicative.Help.Chunk

-- readers --

-- | 'Option' reader based on the 'Read' type class.
auto :: Monad m => Read a => String -> m a
auto arg = case reads arg of
  [(r, "")] -> return r
  _         -> fail $ "cannot parse value `" ++ arg ++ "'"

-- | String 'Option' reader.
str :: Monad m => String -> m String
str = return

-- | Null 'Option' reader. All arguments will fail validation.
disabled :: Monad m => String -> m a
disabled = const . fail $ "disabled option"

-- modifiers --

-- | Specify a short name for an option.
short :: HasName f => Char -> Mod f a
short = fieldMod . name . OptShort

-- | Specify a long name for an option.
long :: HasName f => String -> Mod f a
long = fieldMod . name . OptLong

-- | Specify a default value for an option.
value :: HasValue f => a -> Mod f a
value x = Mod id (DefaultProp (Just x) Nothing) id

-- | Specify a function to show the default value for an option.
showDefaultWith :: (a -> String) -> Mod f a
showDefaultWith s = Mod id (DefaultProp Nothing (Just s)) id

-- | Show the default value for this option using its 'Show' instance.
showDefault :: Show a => Mod f a
showDefault = showDefaultWith show

-- | Specify the help text for an option.
help :: String -> Mod f a
help s = optionMod $ \p -> p { propHelp = paragraph s }

-- | Specify the help text for an option as a 'Text.PrettyPrint.ANSI.Leijen.Doc'
-- value.
helpDoc :: Maybe Doc -> Mod f a
helpDoc doc = optionMod $ \p -> p { propHelp = Chunk doc }

-- | Specify the 'Option' reader.
reader :: (String -> ReadM a) -> Mod OptionFields a
reader f = fieldMod $ \p -> p { optReader = f }

-- | Specify the 'Option' reader as a function in the 'Either' monad.
eitherReader :: (String -> Either String a) -> Mod OptionFields a
eitherReader f = reader (either readerError return . f)

-- | Specify the error to display when no argument is provided to this option.
noArgError :: ParseError -> Mod OptionFields a
noArgError e = fieldMod $ \p -> p { optNoArgError = e }

-- | Specify the metavariable.
metavar :: HasMetavar f => String -> Mod f a
metavar var = optionMod $ \p -> p { propMetaVar = var }

-- | Hide this option from the brief description.
hidden :: Mod f a
hidden = optionMod $ \p ->
  p { propVisibility = min Hidden (propVisibility p) }

-- | Add a command to a subparser option.
command :: String -> ParserInfo a -> Mod CommandFields a
command cmd pinfo = fieldMod $ \p ->
  p { cmdCommands = (cmd, pinfo) : cmdCommands p }

-- | Add a list of possible completion values.
completeWith :: HasCompleter f => [String] -> Mod f a
completeWith xs = completer (listCompleter xs)

-- | Add a bash completion action. Common actions include @file@ and
-- @directory@. See
-- http://www.gnu.org/software/bash/manual/html_node/Programmable-Completion-Builtins.html#Programmable-Completion-Builtins
-- for a complete list.
action :: HasCompleter f => String -> Mod f a
action act = completer (bashCompleter act)

-- | Add a completer to an argument.
--
-- A completer is a function String -> IO String which, given a partial
-- argument, returns all possible completions for that argument.
completer :: HasCompleter f => Completer -> Mod f a
completer f = fieldMod $ modCompleter (`mappend` f)

-- parsers --

-- | Builder for a command parser. The 'command' modifier can be used to
-- specify individual commands.
subparser :: Mod CommandFields a -> Parser a
subparser m = mkParser d g rdr
  where
    Mod _ d g = metavar "COMMAND" `mappend` m
    rdr = uncurry CmdReader (mkCommand m)

-- | Builder for an argument parser.
argument :: (String -> Maybe a) -> Mod ArgumentFields a -> Parser a
argument p (Mod f d g) = mkParser d g (ArgReader rdr)
  where
    ArgumentFields compl = f (ArgumentFields mempty)
    rdr = CReader compl p

-- | Builder for an argument list parser. All arguments are collected and
-- returned as a list.
{-# DEPRECATED arguments "Use many and argument instead" #-}
arguments :: (String -> Maybe a) -> Mod ArgumentFields a -> Parser [a]
arguments r m = many (argument r m)

-- | Like `arguments`, but require at least one argument.
{-# DEPRECATED arguments1 "Use some and argument instead" #-}
arguments1 :: (String -> Maybe a) -> Mod ArgumentFields a -> Parser [a]
arguments1 r m = some (argument r m)

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
nullOption m = mkParser d g rdr
  where
    Mod f d g = metavar "ARG" `mappend` m
    fields = f (OptionFields [] mempty disabled (ErrorMsg ""))
    crdr = CReader (optCompleter fields) (optReader fields)
    rdr = OptReader (optNames fields) crdr (optNoArgError fields)

-- | An option that always fails.
--
-- When this option is encountered, the option parser immediately aborts with
-- the given parse error.  If you simply want to output a message, use
-- 'infoOption' instead.
abortOption :: ParseError -> Mod OptionFields (a -> a) -> Parser (a -> a)
abortOption err m = nullOption . (`mappend` m) $ mconcat
  [ reader (const (ReadM (Left err)))
  , noArgError err
  , value id
  , metavar ""
  , hidden ]

-- | An option that always fails and displays a message.
infoOption :: String -> Mod OptionFields (a -> a) -> Parser (a -> a)
infoOption = abortOption . InfoMsg

-- | Builder for an option taking a 'String' argument.
strOption :: Mod OptionFields String -> Parser String
strOption m = nullOption $ reader str `mappend` m

-- | Builder for an option using the 'auto' reader.
option :: Read a => Mod OptionFields a -> Parser a
option m = nullOption $ reader auto `mappend` m

-- | Modifier for 'ParserInfo'.
newtype InfoMod a = InfoMod
  { applyInfoMod :: ParserInfo a -> ParserInfo a }

instance Monoid (InfoMod a) where
  mempty = InfoMod id
  mappend m1 m2 = InfoMod $ applyInfoMod m2 . applyInfoMod m1

-- | Show a full description in the help text of this parser.
fullDesc :: InfoMod a
fullDesc = InfoMod $ \i -> i { infoFullDesc = True }

-- | Only show a brief description in the help text of this parser.
briefDesc :: InfoMod a
briefDesc = InfoMod $ \i -> i { infoFullDesc = False }

-- | Specify a header for this parser.
header :: String -> InfoMod a
header s = InfoMod $ \i -> i { infoHeader = paragraph s }

-- | Specify a header for this parser as a 'Text.PrettyPrint.ANSI.Leijen.Doc'
-- value.
headerDoc :: Maybe Doc -> InfoMod a
headerDoc doc = InfoMod $ \i -> i { infoHeader = Chunk doc }

-- | Specify a footer for this parser.
footer :: String -> InfoMod a
footer s = InfoMod $ \i -> i { infoFooter = paragraph s }

-- | Specify a footer for this parser as a 'Text.PrettyPrint.ANSI.Leijen.Doc'
-- value.
footerDoc :: Maybe Doc -> InfoMod a
footerDoc doc = InfoMod $ \i -> i { infoFooter = Chunk doc }

-- | Specify a short program description.
progDesc :: String -> InfoMod a
progDesc s = InfoMod $ \i -> i { infoProgDesc = paragraph s }

-- | Specify a short program description as a 'Text.PrettyPrint.ANSI.Leijen.Doc'
-- value.
progDescDoc :: Maybe Doc -> InfoMod a
progDescDoc doc = InfoMod $ \i -> i { infoProgDesc = Chunk doc }

-- | Specify an exit code if a parse error occurs.
failureCode :: Int -> InfoMod a
failureCode n = InfoMod $ \i -> i { infoFailureCode = n }

-- | Disable parsing of regular options after arguments
noIntersperse :: InfoMod a
noIntersperse = InfoMod $ \p -> p { infoIntersperse = False }

-- | Create a 'ParserInfo' given a 'Parser' and a modifier.
info :: Parser a -> InfoMod a -> ParserInfo a
info parser m = applyInfoMod m base
  where
    base = ParserInfo
      { infoParser = parser
      , infoFullDesc = True
      , infoProgDesc = mempty
      , infoHeader = mempty
      , infoFooter = mempty
      , infoFailureCode = 1
      , infoIntersperse = True }

newtype PrefsMod = PrefsMod
  { applyPrefsMod :: ParserPrefs -> ParserPrefs }

instance Monoid PrefsMod where
  mempty = PrefsMod id
  mappend m1 m2 = PrefsMod $ applyPrefsMod m2 . applyPrefsMod m1

multiSuffix :: String -> PrefsMod
multiSuffix s = PrefsMod $ \p -> p { prefMultiSuffix = s }

disambiguate :: PrefsMod
disambiguate = PrefsMod $ \p -> p { prefDisambiguate = True }

showHelpOnError :: PrefsMod
showHelpOnError = PrefsMod $ \p -> p { prefShowHelpOnError = True }

noBacktrack :: PrefsMod
noBacktrack = PrefsMod $ \p -> p { prefBacktrack = False }

columns :: Int -> PrefsMod
columns cols = PrefsMod $ \p -> p { prefColumns = cols }

prefs :: PrefsMod -> ParserPrefs
prefs m = applyPrefsMod m base
  where
    base = ParserPrefs
      { prefMultiSuffix = ""
      , prefDisambiguate = False
      , prefShowHelpOnError = False
      , prefBacktrack = True
      , prefColumns = 80 }

-- convenience shortcuts

-- | Trivial option modifier.
idm :: Monoid m => m
idm = mempty

-- | Compose modifiers.
{-# DEPRECATED (&) "Use (<>) instead" #-}
(&) :: Monoid m => m -> m -> m
(&) = mappend
