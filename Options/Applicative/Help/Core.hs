module Options.Applicative.Help.Core (
  cmdDesc,
  briefDesc,
  fold_tree,
  fullDesc,
  ParserHelp(..),
  errorHelp,
  headerHelp,
  usageHelp,
  bodyHelp,
  footerHelp,
  parserHelp,
  parserUsage,
  ) where

import Control.Monad (guard)
import Data.List (sort, intersperse)
import Data.Maybe (maybeToList, catMaybes)
import Data.Monoid (mempty, mappend)

import Options.Applicative.Common
import Options.Applicative.Types
import Options.Applicative.Help.Pretty
import Options.Applicative.Help.Chunk

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
cmdDesc = vcatChunks . mapParser desc
  where
    desc _ opt =
      case optMain opt of
        CmdReader cmds p ->
          tabulate [(string cmd, align (extractChunk d))
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
fullDesc pprefs = tabulate . catMaybes . mapParser doc
  where
    doc info opt = do
      guard . not . isEmpty $ n
      guard . not . isEmpty $ h
      return (extractChunk n, align . extractChunk $ h <<+>> hdef)
      where
        n = optDesc pprefs style info opt
        h = optHelp opt
        hdef = Chunk . fmap show_def . optShowDefault $ opt
        show_def s = parens (string "default:" <+> string s)
    style = OptDescStyle
      { descSep = string ","
      , descHidden = True
      , descSurround = False }

errorHelp :: Chunk Doc -> ParserHelp
errorHelp chunk = ParserHelp chunk mempty mempty mempty mempty

headerHelp :: Chunk Doc -> ParserHelp
headerHelp chunk = ParserHelp mempty chunk mempty mempty mempty

usageHelp :: Chunk Doc -> ParserHelp
usageHelp chunk = ParserHelp mempty mempty chunk mempty mempty

bodyHelp :: Chunk Doc -> ParserHelp
bodyHelp chunk = ParserHelp mempty mempty mempty chunk mempty

footerHelp :: Chunk Doc -> ParserHelp
footerHelp chunk = ParserHelp mempty mempty mempty mempty chunk

-- | Generate the help text for a program.
parserHelp :: ParserPrefs -> Parser a -> ParserHelp
parserHelp pprefs p = bodyHelp . vsepChunks $
  [ with_title "Available options:" (fullDesc pprefs p)
  , with_title "Available commands:" (cmdDesc p) ]
  where
    with_title :: String -> Chunk Doc -> Chunk Doc
    with_title title = fmap (string title .$.)

-- | Generate option summary.
parserUsage :: ParserPrefs -> Parser a -> String -> Doc
parserUsage pprefs p progn = hsep
  [ string "Usage:"
  , string progn
  , align (extractChunk (briefDesc pprefs p)) ]

{-# ANN footerHelp "HLint: ignore Eta reduce" #-}
