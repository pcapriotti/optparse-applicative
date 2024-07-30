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
import Data.List (sort, intercalate, intersperse)
import Data.Foldable (any, foldl')
import Data.Maybe (fromMaybe, catMaybes)
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (mempty)
#endif
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup (Semigroup (..))
#endif
import Prelude hiding (any)

import Options.Applicative.Common
import Options.Applicative.Internal (groupFstAll)
import Options.Applicative.Types
import Options.Applicative.Help.Pretty
import Options.Applicative.Help.Chunk

-- | Style for rendering an option.
data OptDescStyle
  = OptDescStyle
      { descSep :: Doc,
        descHidden :: Bool,
        descGlobal :: Bool
      }

safelast :: [a] -> Maybe a
safelast = foldl' (const Just) Nothing

-- | Generate description for a single option.
optDesc :: ParserPrefs -> OptDescStyle -> ArgumentReachability -> Option a -> (OptGroup, Chunk Doc, Parenthetic)
optDesc pprefs style _reachability opt =
  let names =
        sort . optionNames . optMain $ opt
      meta =
        stringChunk $ optMetaVar opt
      grp = propGroup $ optProps opt
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
   in (grp, modified, wrapping)

-- | Generate descriptions for commands.
cmdDesc :: ParserPrefs -> Parser a -> [(Maybe String, Chunk Doc)]
cmdDesc pprefs = mapParser desc
  where
    desc _ opt =
      case optMain opt of
        CmdReader gn cmds ->
          (,) gn $
            tabulate (prefTabulateFill pprefs)
              [ (pretty nm, align (extractChunk (infoProgDesc cmd)))
              | (nm, cmd) <- reverse cmds
              ]
        _ -> mempty

-- | Generate a brief help text for a parser.
briefDesc :: ParserPrefs -> Parser a -> Chunk Doc
briefDesc = briefDesc' True

-- | Generate a brief help text for a parser, only including mandatory
--   options and arguments.
missingDesc :: ParserPrefs -> Parser a -> Chunk Doc
missingDesc = briefDesc' False

-- | Generate a brief help text for a parser, allowing the specification
--   of if optional arguments are show.
briefDesc' :: Bool -> ParserPrefs -> Parser a -> Chunk Doc
briefDesc' showOptional pprefs =
  wrapOver NoDefault MaybeRequired
    . foldTree pprefs style
    . mfilterOptional
    . treeMapParser (\a -> (\(_, x, y) -> (x, y)) . optDesc pprefs style a)
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
wrapOver :: AltNodeType -> Parenthetic -> (Chunk Doc, Parenthetic) -> Chunk Doc
wrapOver altnode mustWrapBeyond (chunk, wrapping)
  | altnode == MarkDefault =
    fmap brackets chunk
  | wrapping > mustWrapBeyond =
    fmap parens chunk
  | otherwise =
    chunk

-- Fold a tree of option docs into a single doc with fully marked
-- optional areas and groups.
foldTree :: ParserPrefs -> OptDescStyle -> OptTree (Chunk Doc, Parenthetic) -> (Chunk Doc, Parenthetic)
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
    alt_node :: [(Chunk Doc, Parenthetic)] -> (Chunk Doc, Parenthetic)
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
fullDesc :: ParserPrefs -> Parser a -> Chunk Doc
fullDesc = optionsDesc False

-- | Generate a help text for the parser, showing
--   only what is relevant in the "Global options: section"
globalDesc :: ParserPrefs -> Parser a -> Chunk Doc
globalDesc = optionsDesc True

-- | Common generator for full descriptions and globals
optionsDesc :: Bool -> ParserPrefs -> Parser a -> Chunk Doc
optionsDesc global pprefs p = vsepChunks
  . fmap formatTitle
  . fmap tabulateGroup
  . groupByTitle
  $ docs
  where
    docs :: [Maybe (OptGroup, (Doc, Doc))]
    docs = mapParser doc p

    groupByTitle :: [Maybe (OptGroup, (Doc, Doc))] -> [[(OptGroup, (Doc, Doc))]]
    groupByTitle xs = groupFstAll . catMaybes $ xs

    -- NOTE: [Nested group alignment]
    --
    -- For nested groups, we want to produce output like:
    --
    -- Group 1
    --   --opt-1 INT          Option 1
    --
    -- - Group 2
    --     --opt-2 INT        Option 2
    --
    --   - Group 3
    --       - opt-3 INT      Option 3
    --
    -- That is, we have the following constraints:
    --
    --   1. Nested groups are prefixed with a hyphen '- ', where the hyphen
    --     starts on the same column as the parent group.
    --
    --   2. We still want the listed options to be indented twice under the
    --     group name, so this means nested options need to be indented
    --     again by the standard amount (2), due to the hyphen.
    --
    --   3. Help text should be __globally__ aligned.

    tabulateGroup :: [(OptGroup, (Doc, Doc))] -> (OptGroup, Chunk Doc)
    tabulateGroup l@((title,_):_) =
      (title, tabulate (prefTabulateFill pprefs) (getGroup <$> l))
      where
        -- Handle NOTE: [Nested group alignment] 3. here i.e. indent the
        -- right Doc (help text) according to its indention level and
        -- global maxGroupLevel. Notice there is an inverse relationship here,
        -- as the further the entire group is indented, the less we need to
        -- indent the help text.
        getGroup :: (OptGroup, (Doc, Doc)) -> (Doc, Doc)
        getGroup o@(_, (x, y)) =
          let helpIndent = calcOptHelpIndent o
          in (x, indent helpIndent y)

        -- Indents the option help text, taking the option's group level and
        -- maximum group level into account.
        calcOptHelpIndent :: (OptGroup, a) -> Int
        calcOptHelpIndent g =
          let groupLvl = optGroupToLevel g
          in lvlIndent * (maxGroupLevel - groupLvl)

    tabulateGroup [] = (OptGroup 0 Nothing, mempty)

    formatTitle :: (OptGroup, Chunk Doc) -> Chunk Doc
    formatTitle (OptGroup idx mTitle, opts) =
      -- Two cases to handle w.r.t group level (i.e. nested groups).
      case idx of
        -- Group not nested: no indention.
        0 -> (\d -> pretty title .$. d) <$> opts
        -- Handle NOTE: [Nested group alignment] 1 and 2 here.
        n ->
          let -- indent entire group based on its level.
              indentGroup = indent (lvlIndent * (n - 1))
              -- indent opts an extra lvlIndent to account for hyphen
              indentOpts = indent lvlIndent
          in (\d -> indentGroup $ (pretty $ "- " <> title) .$. indentOpts d)
               <$> opts
      where
        title = case mTitle of
          Nothing -> defTitle
          Just t -> t
        defTitle =
          if global
            then "Global options:"
            else "Available options:"

    maxGroupLevel :: Int
    maxGroupLevel = findMaxGroupLevel docs

    -- Finds the maxium OptGroup level.
    findMaxGroupLevel :: [Maybe (OptGroup, (Doc, Doc))] -> Int
    findMaxGroupLevel = foldl' (\acc -> max acc . optGroupToLevel) 0 . catMaybes

    optGroupToLevel :: (OptGroup, a) -> Int
    optGroupToLevel ((OptGroup i _), _) = i

    doc :: ArgumentReachability -> Option a -> Maybe (OptGroup, (Doc, Doc))
    doc info opt = do
      guard . not . isEmpty $ n
      guard . not . isEmpty $ h
      return (grp, (extractChunk n, align . extractChunk $ h <</>> hdef))
      where
        (grp, n, _) = optDesc pprefs style info opt
        h = optHelp opt
        hdef = Chunk . fmap show_def . optShowDefault $ opt
        show_def s = parens (pretty "default:" <+> pretty s)
    style = OptDescStyle
      { descSep = pretty ',',
        descHidden = True,
        descGlobal = global
      }

    lvlIndent :: Int
    lvlIndent = 2

errorHelp :: Chunk Doc -> ParserHelp
errorHelp chunk = mempty { helpError = chunk }

headerHelp :: Chunk Doc -> ParserHelp
headerHelp chunk = mempty { helpHeader = chunk }

suggestionsHelp :: Chunk Doc -> ParserHelp
suggestionsHelp chunk = mempty { helpSuggestions = chunk }

globalsHelp :: Chunk Doc -> ParserHelp
globalsHelp chunk = mempty { helpGlobals = chunk }

usageHelp :: Chunk Doc -> ParserHelp
usageHelp chunk = mempty { helpUsage = chunk }

descriptionHelp :: Chunk Doc -> ParserHelp
descriptionHelp chunk = mempty { helpDescription = chunk }

bodyHelp :: Chunk Doc -> ParserHelp
bodyHelp chunk = mempty { helpBody = chunk }

footerHelp :: Chunk Doc -> ParserHelp
footerHelp chunk = mempty { helpFooter = chunk }

-- | Generate the help text for a program.
parserHelp :: ParserPrefs -> Parser a -> ParserHelp
parserHelp pprefs p =
  bodyHelp . vsepChunks $
    (fullDesc pprefs p)
      : (group_title <$> cs)
  where
    def = "Available commands:"
    cs = groupFstAll $ cmdDesc pprefs p

    group_title a@((n, _) : _) =
      with_title (fromMaybe def n) $
        vcatChunks (snd <$> a)
    group_title _ = mempty

    with_title :: String -> Chunk Doc -> Chunk Doc
    with_title title = fmap (pretty title .$.)


parserGlobals :: ParserPrefs -> Parser a -> ParserHelp
parserGlobals pprefs p =
  globalsHelp $ globalDesc pprefs p



-- | Generate option summary.
parserUsage :: ParserPrefs -> Parser a -> String -> Doc
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
