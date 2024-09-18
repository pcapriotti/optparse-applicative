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

import           Control.Applicative
import           Control.Monad (guard)

import           Data.Foldable (any, foldl')
import           Data.Function (on)
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (fromMaybe, catMaybes)

import           Prelude hiding (any)

import Options.Applicative.Common
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
        List.sort . optionNames . optMain $ opt
      meta =
        stringChunk $ optMetaVar opt
      grp = propGroup $ optProps opt
      descs =
        map (pretty . showOption) names
      descriptions =
        listToChunk (List.intersperse (descSep style) descs)
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
    . mFilterOptional
    . treeMapParser optDesc'
  where
    mFilterOptional
      | showOptional =
        id
      | otherwise =
        filterOptional
    style = OptDescStyle
      { descSep = pretty '|',
        descHidden = False,
        descGlobal = False
      }
    optDesc' reach opt =
      let
        (_, a, b) =
          optDesc pprefs style reach opt
      in
        (a, b)

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
        multi_wrap xs
   in (x, wrapLevel)
  where
    multi_wrap [_] = NeverRequired
    multi_wrap _ = MaybeRequired
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
optionsDesc global pprefs p =
  vsepChunks
    . formatTitle'
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

    tabulateGroup [] = (OptGroup [], mempty)

    -- Fold so we can update the (printedGroups :: [String]) arg as we
    -- iterate. End with a reverse since we use foldl'.
    formatTitle' :: [(OptGroup, Chunk Doc)] -> [Chunk Doc]
    formatTitle' = reverse . snd . foldl' formatTitle ([], [])

    formatTitle :: ([String], [Chunk Doc]) -> (OptGroup, Chunk Doc) -> ([String], [Chunk Doc])
    formatTitle (printedGroups, acc) o@(OptGroup groups, opts) =
      case parentGroups of
        -- No nested groups: No special logic.
        [] -> (groupTitle : printedGroups, ((\d -> pretty groupTitle .$. d) <$> opts) : acc)
        -- We have at least one parent group title P for current group G: P has
        -- already been printed iff it is attached to another (non-grouped)
        -- option. In other words, P has __not__ been printed if its only
        -- member is another group.
        --
        -- The parameter (printedGroups :: [String]) holds all groups that
        -- have already been printed.
        parents@(_ : _) ->
          let groupLvl = optGroupToLevel o
              -- indent opts an extra lvlIndent to account for hyphen
              indentOpts = indent lvlIndent

              -- new printedGroups is all previous + this and parents.
              printedGroups' = groupTitle : parents ++ printedGroups

              parentsWithIndent = zip [0 .. ] parents

              -- docs for unprinted parent title groups
              parentDocs = pure $ mkParentDocs printedGroups parentsWithIndent

              -- docs for the current group
              thisDocs =
                (\d -> lvlIndentNSub1 groupLvl $ (hyphenate groupTitle) .$. indentOpts d)
                  <$> opts

              allDocs = parentDocs <> thisDocs

          in (printedGroups', allDocs : acc)
      where
        -- Separate parentGroups and _this_ group, in case we need to also
        -- print parent groups.
        (parentGroups, groupTitle) = case unsnoc groups of
          Nothing -> ([], defTitle)
          Just (parentGrps, grp) -> (parentGrps, grp)

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
    -- 0 (defTitle) and 1 (custom group name) are handled identically
    -- w.r.t indenation (not indented). Hence the subtraction here.
    optGroupToLevel (OptGroup [], _) = 0
    optGroupToLevel (OptGroup xs@(_ : _), _) = length xs - 1

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

    --
    -- Prints all parent titles that have not already been printed
    -- (i.e. in printedGroups).
    mkParentDocs :: [String] -> [(Int, String)] -> Doc
    mkParentDocs printedGroups =
        foldr g mempty
      where
        g :: (Int, String) -> Doc ->  Doc
        g (i, s) acc
          | s `List.elem` printedGroups = acc
          | i == 0 = pretty s .$. acc
          | otherwise = lvlIndentNSub1 i $ hyphenate s .$. acc

    hyphenate s = pretty ("- " <> s)

    lvlIndentNSub1 :: Int -> Doc -> Doc
    lvlIndentNSub1 n = indent (lvlIndent * (n - 1))

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
    fullDesc pprefs p
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
        hangAtIfOver 9 (prefBriefHangPoint pprefs) (extractChunk (briefDesc pprefs p))
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

-- | Groups on the first element of the tuple. This differs from the simple
-- @groupBy ((==) `on` fst)@ in that non-adjacent groups are __also__ grouped
-- together. For example:
--
-- @
--   groupFst = groupBy ((==) `on` fst)
--
--   let xs = [(1, "a"), (1, "b"), (3, "c"), (2, "d"), (3, "e"), (2, "f")]
--
--   groupFst xs === [[(1,"a"),(1,"b")],[(3,"c")],[(2,"d")],[(3,"e")],[(2,"f")]]
--   groupFstAll xs === [[(1,"a"),(1,"b")],[(3,"c"),(3,"e")],[(2,"d"),(2,"f")]]
-- @
--
-- Notice that the original order is preserved i.e. we do not first sort on
-- the first element.
--
-- @since 0.19.0.0
groupFstAll :: Ord a => [(a, b)] -> [[(a, b)]]
groupFstAll =
  -- In order to group all (adjacent + non-adjacent) Eq elements together, we
  -- sort the list so that the Eq elements are in fact adjacent, _then_ group.
  -- We don't want to destroy the original order, however, so we add a
  -- temporary index that maintains this original order. The full logic is:
  --
  -- 1. Add index i that preserves original order.
  -- 2. Sort on tuple's fst.
  -- 3. Group by fst.
  -- 4. Sort by i, restoring original order.
  -- 5. Drop index i.
  fmap (NE.toList . dropIdx)
    . List.sortOn toIdx
    . NE.groupBy ((==) `on` fst')
    . List.sortOn fst'
    . zipWithIndex
  where
    dropIdx :: NonEmpty (Int, (a, b)) -> NonEmpty (a, b)
    dropIdx = fmap snd

    toIdx :: NonEmpty (Int, (a, b)) -> Int
    toIdx ((x, _) :| _) = x

    -- Like fst, ignores our added index
    fst' :: (Int, (a, b)) -> a
    fst' (_, (x, _)) = x

    zipWithIndex :: [(a, b)] -> [(Int, (a, b))]
    zipWithIndex = zip [1 ..]

-- | From base-4.19.0.0.
unsnoc :: [a] -> Maybe ([a], a)
unsnoc = foldr (\x -> Just . maybe ([], x) (\(~(a, b)) -> (x : a, b))) Nothing
