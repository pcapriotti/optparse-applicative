{-# LANGUAGE CPP #-}
module Options.Applicative.Help.Core (
  cmdDesc,
  briefDesc,
  missingDesc,
  fullDesc,
  globalDesc,
  ParserHelp(..),
  errorHelp,
  headerHelp,
  suggestionsHelp,
  usageHelp,
  descriptionHelp,
  bodyHelp,
  footerHelp,
  globalsHelp,
  parserHelp,
  parserUsage,
  parserGlobals
  ) where

import Control.Applicative
import Control.Monad (guard)
import Data.Function (on)
import Data.List (sort, intersperse, groupBy)
import Data.Foldable (any, foldl')
import Data.Maybe (maybeToList, catMaybes, fromMaybe)
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (mempty)
#endif
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup (Semigroup (..))
#endif
import Prelude hiding (any)

import Options.Applicative.Common
import Options.Applicative.Types
import Options.Applicative.Help.Pretty
import Options.Applicative.Help.Chunk

-- | Style for rendering an option.
data OptDescStyle ann
  = OptDescStyle
      { descSep :: Doc ann,
        descHidden :: Bool,
        descGlobal :: Bool
      }

safelast :: [a] -> Maybe a
safelast = foldl' (const Just) Nothing

-- | Generate description for a single option.
optDesc :: ParserPrefs -> OptDescStyle ann -> ArgumentReachability -> Option ann a -> (Chunk (Doc ann), Parenthetic)
optDesc pprefs style _reachability opt =
  let names =
        sort . optionNames . optMain $ opt
      meta =
        stringChunk $ optMetaVar opt
      descs =
        map (pretty . showOption) names
      descriptions =
        listToChunk (intersperse (descSep style) descs)
      desc
        | prefHelpLongEquals pprefs && not (isEmpty meta) && any isLongName (safelast names) =
          descriptions <> stringChunk "=" <> meta
        | otherwise =
          descriptions <<+>> meta
      show_opt
        | descGlobal style && not (propShowGlobal (optProps opt)) =
          False
        | optVisibility opt == Hidden =
          descHidden style
        | otherwise =
          optVisibility opt == Visible
      wrapping
        | null names =
          NeverRequired
        | length names == 1 =
          MaybeRequired
        | otherwise =
          AlwaysRequired
      rendered
        | not show_opt =
          mempty
        | otherwise =
          desc
      modified =
        maybe id fmap (optDescMod opt) rendered
   in (modified, wrapping)

-- | Generate descriptions for commands.
cmdDesc :: ParserPrefs -> Parser ann a -> [(Maybe String, Chunk (Doc ann))]
cmdDesc pprefs = mapParser desc
  where
    desc _ opt =
      case optMain opt of
        CmdReader gn cmds p ->
          (,) gn $
            tabulate (prefTabulateFill pprefs)
              [ (pretty cmd, align (extractChunk d))
                | cmd <- reverse cmds,
                  d <- maybeToList . fmap infoProgDesc $ p cmd
              ]
        _ -> mempty

-- | Generate a brief help text for a parser.
briefDesc :: ParserPrefs -> Parser ann a -> Chunk (Doc ann)
briefDesc = briefDesc' True

-- | Generate a brief help text for a parser, only including mandatory
--   options and arguments.
missingDesc :: ParserPrefs -> Parser ann a -> Chunk (Doc ann)
missingDesc = briefDesc' False

-- | Generate a brief help text for a parser, allowing the specification
--   of if optional arguments are show.
briefDesc' :: Bool -> ParserPrefs -> Parser ann a -> Chunk (Doc ann)
briefDesc' showOptional pprefs =
  wrapOver NoDefault MaybeRequired
    . foldTree pprefs style
    . mfilterOptional
    . treeMapParser (optDesc pprefs style)
  where
    mfilterOptional
      | showOptional =
        id
      | otherwise =
        filterOptional
    style = OptDescStyle
      { descSep = pretty "|",
        descHidden = False,
        descGlobal = False
      }

-- | Wrap a doc in parentheses or brackets if required.
wrapOver :: AltNodeType -> Parenthetic -> (Chunk (Doc ann), Parenthetic) -> Chunk (Doc ann)
wrapOver altnode mustWrapBeyond (chunk, wrapping)
  | altnode == MarkDefault =
    fmap brackets chunk
  | wrapping > mustWrapBeyond =
    fmap parens chunk
  | otherwise =
    chunk

-- Fold a tree of option docs into a single doc with fully marked
-- optional areas and groups.
foldTree :: ParserPrefs -> OptDescStyle ann -> OptTree (Chunk (Doc ann), Parenthetic) -> (Chunk (Doc ann), Parenthetic)
foldTree _ _ (Leaf x) =
  x
foldTree prefs s (MultNode xs) =
  let go =
        (<</>>) . wrapOver NoDefault MaybeRequired . foldTree prefs s
      x =
        foldr go mempty xs
      wrapLevel =
        mult_wrap xs
   in (x, wrapLevel)
  where
    mult_wrap [_] = NeverRequired
    mult_wrap _ = MaybeRequired
foldTree prefs s (AltNode b xs) =
  (\x -> (x, NeverRequired))
    . fmap groupOrNestLine
    . wrapOver b MaybeRequired
    . alt_node
    . filter (not . isEmpty . fst)
    . map (foldTree prefs s)
    $ xs
  where
    alt_node :: [(Chunk (Doc ann), Parenthetic)] -> (Chunk (Doc ann), Parenthetic)
    alt_node [n] = n
    alt_node ns =
      (\y -> (y, AlwaysRequired))
        . foldr (chunked altSep . wrapOver NoDefault MaybeRequired) mempty
        $ ns
foldTree prefs s (BindNode x) =
  let rendered =
        wrapOver NoDefault NeverRequired (foldTree prefs s x)

      -- We always want to display the rendered option
      -- if it exists, and only attach the suffix then.
      withSuffix =
        rendered >>= (\r -> pure r <> stringChunk (prefMultiSuffix prefs))
   in (withSuffix, NeverRequired)

-- | Generate a full help text for a parser
fullDesc :: ParserPrefs -> Parser ann a -> Chunk (Doc ann)
fullDesc = optionsDesc False

-- | Generate a help text for the parser, showing
--   only what is relevant in the "Global options: section"
globalDesc :: ParserPrefs -> Parser ann a -> Chunk (Doc ann)
globalDesc = optionsDesc True

-- | Common generator for full descriptions and globals
optionsDesc :: Bool -> ParserPrefs -> Parser ann a -> Chunk (Doc ann)
optionsDesc global pprefs = tabulate (prefTabulateFill pprefs) . catMaybes . mapParser doc
  where
    doc info opt = do
      guard . not . isEmpty $ n
      guard . not . isEmpty $ h
      return (extractChunk n, align . extractChunk $ h <</>> hdef)
      where
        n = fst $ optDesc pprefs style info opt
        h = optHelp opt
        hdef = Chunk . fmap show_def . optShowDefault $ opt
        show_def s = parens (pretty "default:" <+> pretty s)
    style = OptDescStyle
      { descSep = pretty ",",
        descHidden = True,
        descGlobal = global
      }

errorHelp :: Chunk (Doc ann) -> ParserHelp ann
errorHelp chunk = mempty { helpError = chunk }

headerHelp :: Chunk (Doc ann) -> ParserHelp ann
headerHelp chunk = mempty { helpHeader = chunk }

suggestionsHelp :: Chunk (Doc ann) -> ParserHelp ann
suggestionsHelp chunk = mempty { helpSuggestions = chunk }

globalsHelp :: Chunk (Doc ann) -> ParserHelp ann
globalsHelp chunk = mempty { helpGlobals = chunk }

usageHelp :: Chunk (Doc ann) -> ParserHelp ann
usageHelp chunk = mempty { helpUsage = chunk }

descriptionHelp :: Chunk (Doc ann) -> ParserHelp ann
descriptionHelp chunk = mempty { helpDescription = chunk }

bodyHelp :: Chunk (Doc ann) -> ParserHelp ann
bodyHelp chunk = mempty { helpBody = chunk }

footerHelp :: Chunk (Doc ann) -> ParserHelp ann
footerHelp chunk = mempty { helpFooter = chunk }

-- | Generate the help text for a program.
parserHelp :: ParserPrefs -> Parser ann a -> ParserHelp ann
parserHelp pprefs p =
  bodyHelp . vsepChunks $
    with_title "Available options:" (fullDesc pprefs p)
      : (group_title <$> cs)
  where
    def = "Available commands:"
    cs = groupBy ((==) `on` fst) $ cmdDesc pprefs p

    group_title a@((n, _) : _) =
      with_title (fromMaybe def n) $
        vcatChunks (snd <$> a)
    group_title _ = mempty

    with_title :: String -> Chunk (Doc ann) -> Chunk (Doc ann)
    with_title title = fmap (pretty title .$.)


parserGlobals :: ParserPrefs -> Parser ann a -> ParserHelp ann
parserGlobals pprefs p =
  globalsHelp $
    (.$.) <$> stringChunk "Global options:"
          <*> globalDesc pprefs p



-- | Generate option summary.
parserUsage :: ParserPrefs -> Parser ann a -> String -> Doc ann
parserUsage pprefs p progn =
  hsep
    [ pretty "Usage:",
      pretty progn,
      align (extractChunk (briefDesc pprefs p))
    ]

-- | Peek at the structure of the rendered tree within.
--
--   For example, if a child is an option with multiple
--   alternatives, such as -a or -b, we need to know this
--   when wrapping it. For example, whether it's optional:
--   we don't want to have [(-a|-b)], rather [-a|-b] or
--   (-a|-b).
data Parenthetic
  = NeverRequired
  -- ^ Parenthesis are not required.
  | MaybeRequired
  -- ^ Parenthesis should be used if this group can be repeated
  | AlwaysRequired
  -- ^ Parenthesis should always be used.
  deriving (Eq, Ord, Show)
