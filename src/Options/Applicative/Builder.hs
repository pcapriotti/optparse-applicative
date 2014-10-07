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
  strArgument,
  argument,
  flag,
  flag',
  switch,
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

import Control.Applicative
import Data.Monoid (Monoid (..)
#if __GLASGOW_HASKELL__ > 702
  , (<>)
#endif
  )

import Options.Applicative.Builder.Completer
import Options.Applicative.Builder.Internal
import Options.Applicative.Basic
import Options.Applicative.Classes
import Options.Applicative.Help.Pretty
import Options.Applicative.Help.Chunk
import Options.Applicative.Types
import Options.Applicative.WithArgs

-- readers --

-- | 'Option' reader based on the 'Read' type class.
auto :: Read a => ReadM a
auto = eitherReader $ \arg -> case reads arg of
  [(r, "")] -> return r
  _         -> Left $ "cannot parse value `" ++ arg ++ "'"

-- | String 'Option' reader.
str :: ReadM String
str = readerAsk

-- | Null 'Option' reader. All arguments will fail validation.
disabled :: ReadM a
disabled = readerError "disabled option"

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

-- | Convert a function in the 'Either' monad to a reader.
eitherReader :: (String -> Either String a) -> ReadM a
eitherReader f = readerAsk >>= either readerError return . f

-- | Specify the error to display when no argument is provided to this option.
noArgError :: ParseError -> Mod OptionFields a
noArgError e = fieldMod $ \p -> p { optNoArgError = e }

-- | Specify a metavariable for the argument.
--
-- Metavariables have no effect on the actual parser, and only serve to specify
-- the symbolic name for an argument to be displayed in the help text.
metavar :: HasMetavar f => String -> Mod f a
metavar var = optionMod $ \p -> p { propMetaVar = var }

-- | Hide this option from the brief description.
hidden :: Mod f a
hidden = optionMod $ \p ->
  p { propVisibility = min Hidden (propVisibility p) }

-- | Add a command to a subparser option.
command :: (Applicative f, HasCommand f)
        => String -> WithSub f a -> WithSub f a
command cmd sub = wrapSub (mkCommand cmd sub)

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

-- | Builder for an argument parser.
argument :: HasArgument f
         => ReadM a -> Mod ArgumentFields a -> f a
argument p (Mod _ d g) = mkArgument (mkProps d g) p

-- | Builder for a 'String' argument.
strArgument :: HasArgument f => Mod ArgumentFields String -> f String
strArgument = argument str

-- | Builder for a flag parser.
--
-- A flag that switches from a \"default value\" to an \"active value\" when
-- encountered. For a simple boolean value, use `switch` instead.
flag :: (HasFlag f, Alternative f)
     => a                         -- ^ default value
     -> a                         -- ^ active value
     -> Mod FlagFields a          -- ^ option modifier
     -> f a
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
flag' :: (Alternative f, HasFlag f)
      => a                         -- ^ active value
      -> Mod FlagFields a          -- ^ option modifier
      -> f a
flag' x (Mod f d g) = mkFlag (mkProps d g) (flagNames fields) x
  where
    fields = f (FlagFields [])

-- | Builder for a boolean flag.
--
-- > switch = flag False True
switch :: (Alternative f, HasFlag f) => Mod FlagFields Bool -> f Bool
switch = flag False True

-- | An option that always fails.
--
-- When this option is encountered, the option parser immediately aborts with
-- the given parse error.  If you simply want to output a message, use
-- 'infoOption' instead.
abortOption :: HasOption f => ParseError -> Mod OptionFields a -> f a
abortOption err m = option (readerAbort err) . (`mappend` m) $ mconcat
  [ noArgError err , metavar "" ]

-- | An option that always fails and displays a message.
infoOption :: HasOption f => String -> Mod OptionFields a -> f a
infoOption = abortOption . InfoMsg

-- | Builder for an option taking a 'String' argument.
strOption :: HasOption f => Mod OptionFields String -> f String
strOption = option str

-- | Builder for an option using the 'auto' reader.
option :: HasOption f => ReadM a -> Mod OptionFields a -> f a
option r m = mkOption (mkProps d g) (optNames fields) r
  where
    Mod f d g = metavar "ARG" `mappend` m
    fields = f (OptionFields [] mempty (ErrorMsg ""))

-- | Modifier for 'ParserInfo'.
newtype InfoMod a = InfoMod
  { applyInfoMod :: ParserInfo a -> ParserInfo a }

instance Monoid (InfoMod a) where
  mempty = InfoMod id
  mappend m1 m2 = InfoMod $ applyInfoMod m2 . applyInfoMod m1

-- | Show a full description in the help text of this parser.
fullDesc :: InfoMod a
fullDesc = InfoMod . overMetadata $ \i -> i { mdFullDesc = True }

-- | Only show a brief description in the help text of this parser.
briefDesc :: InfoMod a
briefDesc = InfoMod . overMetadata $ \i -> i { mdFullDesc = False }

-- | Specify a header for this parser.
header :: String -> InfoMod a
header s = InfoMod . overMetadata $ \i -> i { mdHeader = paragraph s }

-- | Specify a header for this parser as a 'Text.PrettyPrint.ANSI.Leijen.Doc'
-- value.
headerDoc :: Maybe Doc -> InfoMod a
headerDoc doc = InfoMod . overMetadata $ \i -> i { mdHeader = Chunk doc }

-- | Specify a footer for this parser.
footer :: String -> InfoMod a
footer s = InfoMod . overMetadata $ \i -> i { mdFooter = paragraph s }

-- | Specify a footer for this parser as a 'Text.PrettyPrint.ANSI.Leijen.Doc'
-- value.
footerDoc :: Maybe Doc -> InfoMod a
footerDoc doc = InfoMod . overMetadata $ \i -> i { mdFooter = Chunk doc }

-- | Specify a short program description.
progDesc :: String -> InfoMod a
progDesc s = InfoMod . overMetadata $ \i -> i { mdProgDesc = paragraph s }

-- | Specify a short program description as a 'Text.PrettyPrint.ANSI.Leijen.Doc'
-- value.
progDescDoc :: Maybe Doc -> InfoMod a
progDescDoc doc = InfoMod . overMetadata $ \i -> i { mdProgDesc = Chunk doc }

-- | Specify an exit code if a parse error occurs.
failureCode :: Int -> InfoMod a
failureCode n = InfoMod . overMetadata $ \i -> i { mdFailureCode = n }

-- | Disable parsing of regular options after arguments
noIntersperse :: InfoMod a
noIntersperse = InfoMod . overMetadata $ \p -> p { mdIntersperse = False }

-- | Create a 'ParserInfo' given a 'Parser' and a modifier.
info :: Parser a -> InfoMod a -> ParserInfo a
info parser m = applyInfoMod m (WithInfo md parser)
  where
    md = Metadata
      { mdFullDesc = True
      , mdProgDesc = mempty
      , mdHeader = mempty
      , mdFooter = mempty
      , mdFailureCode = 1
      , mdIntersperse = True }

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
