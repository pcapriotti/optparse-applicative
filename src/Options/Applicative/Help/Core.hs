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
import Data.Maybe (catMaybes, fromMaybe)
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
import Options.Applicative.Help.HelpDoc (HelpDoc, HelpType (CmdName, Description, Metavar, OptionName, Title), annotateHelp, ansiDocToHelpDoc)

-- XXX(Martin): Seems like this module returns a bunch of helpers for generating Help, there is no
--   single top level function, instead they are called and combined together in
--   Options.Applicative.Extra? Makes it a bit hard to reason about the whole thing for me,
--   plus module named Extra sounds like it wouldn't be doing this, combining them.

-- | Style for rendering an option.
data OptDescStyle
  = OptDescStyle
      { descSep :: HelpDoc,
        descHidden :: Bool,
        descGlobal :: Bool
      }

safelast :: [a] -> Maybe a
safelast = foldl' (const Just) Nothing

-- XXX(Martin): What does this really generate? Just the names for the option + metavar?
--   Or does it also generate its usage information? I don't see where usage is getting generated.
--   I guess the question is, what does `Desc` in `optDesc` stand for? What kind of description?

-- | Generate description for a single option.
optDesc :: ParserPrefs -> OptDescStyle -> ArgumentReachability -> Option a -> (Chunk HelpDoc, Parenthetic)
optDesc pprefs style _reachability opt =
  let names =
        sort . optionNames . optMain $ opt
      meta =
        annotateHelp Metavar <$> stringChunk (optMetaVar opt)
      descs =
        map (annotateHelp OptionName . pretty . showOption) names
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
        -- TODO(martin): optDescMod here wants to modify the description (`rendered`), but we made
        --   description HelpDoc, because we are annotating stuff like metavar and option name,
        --   while optDescMode works as AnsiDoc -> AnsiDoc. Figure out what to do: do we make
        --   optDescMode work as HelpDoc -> HelpDoc, or do we give up annotating description here
        --   and make it AnsiDoc, or maybe there is third option? I am leaning toward making
        --   optDescMode operate as HelpDoc -> HelpDoc, but I don't yet understand the whole
        --   situation.
        maybe id fmap (optDescMod opt) rendered
   in (modified, wrapping)

-- TODO(Martin): I started going through this file and annotating chunks,
--   but there is still more to annotate and I am not having an easy time figuring out what
--   is what in the codebase, so it goes very slow.

-- | Generate descriptions for commands.
cmdDesc :: ParserPrefs -> Parser a -> [(Maybe String, Chunk HelpDoc)]
cmdDesc pprefs = mapParser desc
  where
    desc _ opt =
      case optMain opt of
        CmdReader gn cmds ->
          (,) gn $
            tabulate (prefTabulateFill pprefs)
              [ (annotateHelp CmdName $ pretty cmdName,
                 align (annotateHelp Description $ ansiDocToHelpDoc $ extractChunk (infoProgDesc cmdInfo))
                )
              | (cmdName, cmdInfo) <- reverse cmds
              ]
        _ -> mempty

-- | Generate a brief help text for a parser.
briefDesc :: ParserPrefs -> Parser a -> Chunk HelpDoc
briefDesc = briefDesc' True

-- | Generate a brief help text for a parser, only including mandatory
--   options and arguments.
missingDesc :: ParserPrefs -> Parser a -> Chunk HelpDoc
missingDesc = briefDesc' False

-- | Generate a brief help text for a parser, allowing the specification
--   of if optional arguments are show.
briefDesc' :: Bool -> ParserPrefs -> Parser a -> Chunk HelpDoc
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
      { descSep = pretty '|',
        descHidden = False,
        descGlobal = False
      }

-- | Wrap a doc in parentheses or brackets if required.
wrapOver :: AltNodeType -> Parenthetic -> (Chunk HelpDoc, Parenthetic) -> Chunk HelpDoc
wrapOver altnode mustWrapBeyond (chunk, wrapping)
  | altnode == MarkDefault =
    fmap brackets chunk
  | wrapping > mustWrapBeyond =
    fmap parens chunk
  | otherwise =
    chunk

-- Fold a tree of option docs into a single doc with fully marked
-- optional areas and groups.
foldTree :: ParserPrefs -> OptDescStyle -> OptTree (Chunk HelpDoc, Parenthetic) -> (Chunk HelpDoc, Parenthetic)
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
    alt_node :: [(Chunk HelpDoc, Parenthetic)] -> (Chunk HelpDoc, Parenthetic)
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
fullDesc :: ParserPrefs -> Parser a -> Chunk HelpDoc
fullDesc = optionsDesc False

-- | Generate a help text for the parser, showing
--   only what is relevant in the "Global options: section"
globalDesc :: ParserPrefs -> Parser a -> Chunk HelpDoc
globalDesc = optionsDesc True

-- | Common generator for full descriptions and globals
optionsDesc :: Bool -> ParserPrefs -> Parser a -> Chunk HelpDoc
optionsDesc global pprefs = tabulate (prefTabulateFill pprefs) . catMaybes . mapParser doc
  where
    doc info opt = do
      guard . not . isEmpty $ n
      guard . not . isEmpty $ h
      return (extractChunk n, align . extractChunk $ h <</>> hdef)
      where
        n = fst $ optDesc pprefs style info opt
        -- TODO(Martin) Not 100% if this `ansiDocToHelpDoc` makes sense here as a move, should double check.
        h = ansiDocToHelpDoc <$> optHelp opt
        hdef = Chunk . fmap show_def . optShowDefault $ opt
        show_def s = parens (pretty "default:" <+> pretty s)
    style = OptDescStyle
      { descSep = pretty ',',
        descHidden = True,
        descGlobal = global
      }

errorHelp :: Chunk HelpDoc -> ParserHelp
errorHelp chunk = mempty { helpError = chunk }

headerHelp :: Chunk HelpDoc -> ParserHelp
headerHelp chunk = mempty { helpHeader = chunk }

suggestionsHelp :: Chunk HelpDoc -> ParserHelp
suggestionsHelp chunk = mempty { helpSuggestions = chunk }

globalsHelp :: Chunk HelpDoc -> ParserHelp
globalsHelp chunk = mempty { helpGlobals = chunk }

usageHelp :: Chunk HelpDoc -> ParserHelp
usageHelp chunk = mempty { helpUsage = chunk }

descriptionHelp :: Chunk HelpDoc -> ParserHelp
descriptionHelp chunk = mempty { helpDescription = chunk }

bodyHelp :: Chunk HelpDoc -> ParserHelp
bodyHelp chunk = mempty { helpBody = chunk }

footerHelp :: Chunk HelpDoc -> ParserHelp
footerHelp chunk = mempty { helpFooter = chunk }

-- | Generate the help text for a program.
parserHelp :: ParserPrefs -> Parser a -> ParserHelp
parserHelp pprefs p =
  bodyHelp . vsepChunks $
    optionsHelp :
    (cmdGroupHelp <$> cmdGroups)
  where
    optionsHelp = with_title "Available options:" (fullDesc pprefs p)

    cmdGroupHelp cmdGroup@((groupName, _) : _) =
      with_title (fromMaybe "Available commands:" groupName) $
        vcatChunks (snd <$> cmdGroup)
    cmdGroupHelp _ = mempty

    cmdGroups = groupBy ((==) `on` fst) $ cmdDesc pprefs p

    with_title :: String -> Chunk HelpDoc -> Chunk HelpDoc
    with_title title = fmap (annotateHelp Title . (pretty title .$.))


parserGlobals :: ParserPrefs -> Parser a -> ParserHelp
parserGlobals pprefs p =
  globalsHelp $
    (.$.) <$> stringChunk "Global options:"
          <*> globalDesc pprefs p



-- | Generate option summary.
parserUsage :: ParserPrefs -> Parser a -> String -> HelpDoc
parserUsage pprefs p progn =
  group $
    hsep
      [ pretty "Usage:",
        pretty progn,
        hangAtIfOver 9 35 (extractChunk (briefDesc pprefs p))
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
