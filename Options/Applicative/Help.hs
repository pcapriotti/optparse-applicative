module Options.Applicative.Help (
  cmdDesc,
  briefDesc,
  fullDesc,
  ParserHelp(..),
  helpText,
  parserHelp,
  headerHelp,
  usageHelp,
  module Text.PrettyPrint.ANSI.Leijen
  ) where

import Control.Monad (guard)
import Data.List (intersperse, sort)
import Data.Maybe (maybeToList, catMaybes)
import Data.Monoid (Monoid, mempty, mappend, mconcat)

import Options.Applicative.Common
import Options.Applicative.Types
import Options.Applicative.Utils

import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

-- | Style for rendering an option.
data OptDescStyle = OptDescStyle
  { descSep :: Doc
  , descHidden :: Bool
  , descSurround :: Bool }

-- | Generate description for a single option.
optDesc :: ParserPrefs -> OptDescStyle -> OptHelpInfo -> Option a -> Chunk Doc
optDesc pprefs style info opt =
  let ns = optionNames $ optMain opt
      mv = stringChunk $ optMetaVar opt
      descs = map (string . showOption) (sort ns)
      desc' = listToChunk (intersperse (descSep style) descs) <<+>> mv
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
  in render desc'

-- | Generate descriptions for commands.
cmdDesc :: Parser a -> Chunk Doc
cmdDesc = mconcat . mapParser desc
  where
    desc _ opt =
      case optMain opt of
        CmdReader cmds p ->
          tabulate [(string cmd, string d)
                   | cmd <- reverse cmds
                   , d <- maybeToList . fmap infoProgDesc $ p cmd ]
        _ -> mempty

-- | Generate a brief help text for a parser.
briefDesc :: ParserPrefs -> Parser a -> Chunk Doc
briefDesc pprefs = fold_tree . treeMapParser (optDesc pprefs style)
  where
    style = OptDescStyle
      { descSep = string "|"
      , descHidden = False
      , descSurround = True }

    fold_tree (Leaf x) = x
    fold_tree (MultNode xs) = foldr (<<+>>) mempty . map fold_tree $ xs
    fold_tree (AltNode xs) = alt_node
                           . filter (not . isEmpty)
                           . map fold_tree $ xs

    alt_node :: [Chunk Doc] -> Chunk Doc
    alt_node [n] = n
    alt_node ns = fmap parens
                . foldr (chunked (mappendWith (string " | "))) mempty
                $ ns

-- | Generate a full help text for a parser.
fullDesc :: ParserPrefs -> Parser a -> Chunk Doc
fullDesc pprefs = tabulate . catMaybes . mapParser doc
  where
    doc info opt = do
      guard . not . isEmpty $ n
      guard . not . isEmpty $ h
      return (extract n, extract (h <<+>> hdef))
      where
        n = optDesc pprefs style info opt
        h = stringChunk . optHelp $ opt
        hdef = Chunk . fmap show_def . optShowDefault $ opt
        show_def s = parens (string "default:" <+> string s)
    style = OptDescStyle
      { descSep = string ","
      , descHidden = True
      , descSurround = False }

data ParserHelp = ParserHelp
  { helpHeader :: Chunk Doc
  , helpUsage :: Chunk Doc
  , helpBody :: Chunk Doc
  , helpFooter :: Chunk Doc }

instance Monoid ParserHelp where
  mempty = ParserHelp mempty mempty mempty mempty
  mappend (ParserHelp h1 u1 b1 f1) (ParserHelp h2 u2 b2 f2)
    = ParserHelp (mappend h1 h2) (mappend u1 u2)
                 (mappend b1 b2) (mappend f1 f2)

headerHelp :: Chunk Doc -> ParserHelp
headerHelp chunk = ParserHelp chunk mempty mempty mempty

usageHelp :: Chunk Doc -> ParserHelp
usageHelp chunk = ParserHelp mempty chunk mempty mempty

helpText :: ParserHelp -> Doc
helpText (ParserHelp h u b f) = extract . vsepChunks $ [h, u, b, f]

-- | Generate the help text for a program.
parserHelp :: ParserPrefs -> ParserInfo a -> ParserHelp
parserHelp pprefs pinfo = ParserHelp
  { helpHeader = stringChunk . infoHeader $ pinfo
  , helpUsage = mempty
  , helpBody = vsepChunks
      [ do guard (infoFullDesc pinfo)
           doc <- fullDesc pprefs p
           return $ vsep [string "Available options:", doc]
      , do guard (infoFullDesc pinfo)
           doc <- cmdDesc p
           return $ vsep [string "Available commands:", doc] ]
  , helpFooter = stringChunk (infoFooter pinfo) }
  where
    p = infoParser pinfo
