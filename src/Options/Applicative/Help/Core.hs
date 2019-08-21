module Options.Applicative.Help.Core (
  cmdDesc,
  briefDesc,
  missingDesc,
  fullDesc,
  ParserHelp(..),
  errorHelp,
  headerHelp,
  suggestionsHelp,
  usageHelp,
  bodyHelp,
  footerHelp,
  parserHelp,
  parserUsage,
  ) where

import Control.Applicative
import Control.Monad (guard)
import Data.Function (on)
import Data.List (sort, intersperse, groupBy)
import Data.Foldable (any)
import Data.Maybe (maybeToList, catMaybes, fromMaybe)
import Data.Monoid (mempty)
import Data.Semigroup (Semigroup (..))
import Prelude hiding (any)

import Options.Applicative.Common
import Options.Applicative.Types
import Options.Applicative.Help.Pretty
import Options.Applicative.Help.Chunk

-- | Style for rendering an option.
data OptDescStyle = OptDescStyle
  { descSep :: Doc
  , descHidden :: Bool }

safelast :: [a] -> Maybe a
safelast = foldl (const Just) Nothing

-- | Generate description for a single option.
optDesc :: ParserPrefs -> OptDescStyle -> OptHelpInfo -> Option a -> (Chunk Doc, Wrapping)
optDesc pprefs style info opt =
  let names
        = sort . optionNames . optMain $ opt
      meta
        = stringChunk $ optMetaVar opt
      descs
        = map (string . showOption) names
      descriptions
        = listToChunk (intersperse (descSep style) descs)
      desc
        | prefHelpLongEquals pprefs && not (isEmpty meta) && any isLongName (safelast names)
        = descriptions <> stringChunk "=" <> meta
        | otherwise
        = descriptions <<+>> meta
      show_opt
        | optVisibility opt == Hidden
        = descHidden style
        | otherwise
        = optVisibility opt == Visible
      suffix
        | hinfoMulti info
        = stringChunk . prefMultiSuffix $ pprefs
        | otherwise
        = mempty
      wrapping
        = wrapIf (length names > 1)
      rendered
        | not show_opt
        = mempty
        | otherwise
        = desc <> suffix
      modified
        = maybe id fmap (optDescMod opt) rendered
  in  (modified, wrapping)

-- | Generate descriptions for commands.
cmdDesc :: Parser a -> [(Maybe String, Chunk Doc)]
cmdDesc = mapParser desc
  where
    desc _ opt =
      case optMain opt of
        CmdReader gn cmds p -> (,) gn $
          tabulate [(string cmd, align (extractChunk d))
                   | cmd <- reverse cmds
                   , d <- maybeToList . fmap infoProgDesc $ p cmd ]
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
briefDesc' showOptional pprefs
    = wrap NoDefault . foldTree . mfilterOptional . treeMapParser (optDesc pprefs style)
  where
    mfilterOptional
      | showOptional
      = id
      | otherwise
      = filterOptional

    style = OptDescStyle
      { descSep = string "|"
      , descHidden = False }

-- | Wrap a doc in parentheses or brackets if required.
wrap :: AltNodeType -> (Chunk Doc, Wrapping) -> Chunk Doc
wrap altnode (chunk, wrapping)
  | altnode == MarkDefault
  = fmap brackets chunk
  | needsWrapping wrapping
  = fmap parens chunk
  | otherwise
  = chunk

-- Fold a tree of option docs into a single doc with fully marked
-- optional areas and groups.
foldTree :: OptTree (Chunk Doc, Wrapping) -> (Chunk Doc, Wrapping)
foldTree (Leaf x)
  = x
foldTree (MultNode xs)
  = (foldr ((<</>>) . wrap NoDefault . foldTree) mempty xs, Bare)
foldTree (AltNode b xs)
  = (\x -> (x, Bare))
  . fmap groupOrNestLine
  . wrap b
  . alt_node
  . filter (not . isEmpty . fst)
  . map foldTree $ xs
    where

  alt_node :: [(Chunk Doc, Wrapping)] -> (Chunk Doc, Wrapping)
  alt_node [n] = n
  alt_node ns = (\y -> (y, Wrapped))
              . foldr (chunked altSep . wrap NoDefault) mempty
              $ ns

-- | Generate a full help text for a parser.
fullDesc :: ParserPrefs -> Parser a -> Chunk Doc
fullDesc pprefs = tabulate . catMaybes . mapParser doc
  where
    doc info opt = do
      guard . not . isEmpty $ n
      guard . not . isEmpty $ h
      return (extractChunk n, align . extractChunk $ h <<+>> hdef)
      where
        n = fst $ optDesc pprefs style info opt
        h = optHelp opt
        hdef = Chunk . fmap show_def . optShowDefault $ opt
        show_def s = parens (string "default:" <+> string s)
    style = OptDescStyle
      { descSep = string ","
      , descHidden = True }

errorHelp :: Chunk Doc -> ParserHelp
errorHelp chunk = mempty { helpError = chunk }

headerHelp :: Chunk Doc -> ParserHelp
headerHelp chunk = mempty { helpHeader = chunk }

suggestionsHelp :: Chunk Doc -> ParserHelp
suggestionsHelp chunk = mempty { helpSuggestions = chunk }

usageHelp :: Chunk Doc -> ParserHelp
usageHelp chunk = mempty { helpUsage = chunk }

bodyHelp :: Chunk Doc -> ParserHelp
bodyHelp chunk = mempty { helpBody = chunk }

footerHelp :: Chunk Doc -> ParserHelp
footerHelp chunk = mempty { helpFooter = chunk }

-- | Generate the help text for a program.
parserHelp :: ParserPrefs -> Parser a -> ParserHelp
parserHelp pprefs p = bodyHelp . vsepChunks
  $ with_title "Available options:" (fullDesc pprefs p)
  : (group_title <$> cs)
  where
    def = "Available commands:"

    cs = groupBy ((==) `on` fst) $ cmdDesc p

    group_title a@((n,_):_) = with_title (fromMaybe def n) $
      vcatChunks (snd <$> a)
    group_title _ = mempty


    with_title :: String -> Chunk Doc -> Chunk Doc
    with_title title = fmap (string title .$.)

-- | Generate option summary.
parserUsage :: ParserPrefs -> Parser a -> String -> Doc
parserUsage pprefs p progn = hsep
  [ string "Usage:"
  , string progn
  , align (extractChunk (briefDesc pprefs p)) ]

data Wrapping
  = Bare
  | Wrapped
  deriving (Eq, Show)

wrapIf :: Bool -> Wrapping
wrapIf b = if b then Wrapped else Bare

needsWrapping :: Wrapping -> Bool
needsWrapping = (==) Wrapped
