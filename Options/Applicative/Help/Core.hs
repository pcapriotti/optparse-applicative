{-# LANGUAGE TupleSections #-}

module Options.Applicative.Help.Core (
  cmdDesc,
  briefDesc,
  missingDesc,
  fold_tree,
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
import Data.List (sort, intersperse, groupBy, sortBy)
import Data.Maybe (maybeToList, catMaybes, fromMaybe)
import Data.Monoid
import Prelude

import Options.Applicative.Common
import Options.Applicative.Types
import Options.Applicative.Help.Pretty
import Options.Applicative.Help.Chunk

-- | Style for rendering an option.
data OptDescStyle = OptDescStyle
  { descSep :: Doc
  , descHidden :: Bool
  , descOptional :: Bool
  , descSurround :: Bool }

optionGroup :: OptReader a -> Maybe String
optionGroup (OptReader gn _ _ _) = gn
optionGroup (ArgReader gn _) = gn
optionGroup (FlagReader gn _ _) = gn
optionGroup (CmdReader gn _ _) = gn

-- | Generate description for a single option.
optDesc :: ParserPrefs -> OptDescStyle -> OptHelpInfo -> Option a -> (Maybe String, Chunk Doc)
optDesc pprefs style info opt =
  let ns = optionNames $ optMain opt
      grp = optionGroup $ optMain opt
      mv = stringChunk $ optMetaVar opt
      descs = map (string . showOption) (sort ns)
      desc' = listToChunk (intersperse (descSep style) descs) <<+>> mv
      show_opt
        | hinfoDefault info && not (descOptional style)
        = False
        | optVisibility opt == Hidden
        = descHidden style
        | otherwise
        = optVisibility opt == Visible
      suffix
        | hinfoMulti info
        = stringChunk . prefMultiSuffix $ pprefs
        | otherwise
        = mempty
      render chunk
        | not show_opt
        = mempty
        | isEmpty chunk || not (descSurround style)
        = mappend chunk suffix
        | hinfoDefault info
        = mappend (fmap brackets chunk) suffix
        | null (drop 1 descs)
        = mappend chunk suffix
        | otherwise
        = mappend (fmap parens chunk) suffix
  in (grp, maybe id fmap (optDescMod opt) (render desc'))

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
briefDesc' showOptional pprefs = fold_tree . treeMapParser (\a -> snd . optDesc pprefs style a)
  where
    style = OptDescStyle
      { descSep = string "|"
      , descHidden = False
      , descOptional = showOptional
      , descSurround = True }

fold_tree :: OptTree (Chunk Doc) -> Chunk Doc
fold_tree (Leaf x) = x
fold_tree (MultNode xs) = foldr ((<</>>) . fold_tree) mempty xs
fold_tree (AltNode xs) = alt_node
                       . filter (not . isEmpty)
                       . map fold_tree $ xs
  where
    alt_node :: [Chunk Doc] -> Chunk Doc
    alt_node [n] = n
    alt_node ns = fmap parens
                . foldr (chunked (\x y -> x </> char '|' </> y)) mempty
                $ ns

-- | Generate a full help text for a parser.
fullDesc :: ParserPrefs -> Parser a -> Chunk Doc
fullDesc pprefs p = vsepChunks
    . fmap formatTitle
    . fmap tabulateGroup
    . groupByTitle
    $ mapParser doc p
  where
    groupByTitle = sortBy (compare `on` (fst . head)) . groupBy ((==) `on` fst) . catMaybes

    tabulateGroup l@((title,_):_) = (title, tabulate (snd <$> l))
    tabulateGroup [] = mempty

    def_title = "Available options:"
    formatTitle (title, opts) = (string (fromMaybe def_title title) .$.) <$> opts

    doc info opt = do
      guard . not . isEmpty $ n
      guard . not . isEmpty $ h
      return (grp, (extractChunk n, align . extractChunk $ h <<+>> hdef))
      where
        (grp, n) = optDesc pprefs style info opt
        h = optHelp opt
        hdef = Chunk . fmap show_def . optShowDefault $ opt
        show_def s = parens (string "default:" <+> string s)

    style = OptDescStyle
      { descSep = string ","
      , descHidden = True
      , descOptional = True
      , descSurround = False }

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
parserHelp pprefs p = bodyHelp . vsepChunks $
  fullDesc pprefs p : cmds
  where
    cmd_def = "Available commands:"
    cmds = cmd_with_title <$> (groupBy ((==) `on` fst) $ cmdDesc p)

    cmd_with_title a@((n,_):_) = with_title (fromMaybe cmd_def n) $
      vcatChunks (snd <$> a)
    cmd_with_title _ = mempty

    with_title :: String -> Chunk Doc -> Chunk Doc
    with_title title = fmap (string title .$.)

-- | Generate option summary.
parserUsage :: ParserPrefs -> Parser a -> String -> Doc
parserUsage pprefs p progn = hsep
  [ string "Usage:"
  , string progn
  , align (extractChunk (briefDesc pprefs p)) ]

{-# ANN footerHelp "HLint: ignore Eta reduce" #-}
