module Options.Applicative.Help.Core where
--   cmdDesc,
--   briefDesc,
--   fullDesc,
--   ParserHelp(..),
--   errorHelp,
--   headerHelp,
--   usageHelp,
--   bodyHelp,
--   footerHelp,
--   parserHelp,
--   parserUsage,
--   ) where

import Data.Monoid

import Options.Applicative.Basic
import Options.Applicative.Types
import Options.Applicative.Help.Pretty
import Options.Applicative.Help.Chunk

-- | Style for rendering an option.
data OptDescStyle = OptDescStyle
  { descSep :: Doc
  , descHidden :: Bool
  , descSurround :: Bool }

-- | Generate descriptions for commands.
cmdDesc :: f a -> Chunk Doc
cmdDesc = mempty
--   where
--     desc _ opt =
--       case optMain opt of
--         CmdReader cmds p ->
--           tabulate [(string cmd, align (extractChunk d))
--                    | cmd <- reverse cmds
--                    , d <- maybeToList . fmap infoProgDesc $ p cmd ]
--         _ -> mempty

-- | Generate a full help text for a parser.
fullDesc :: f a -> Chunk Doc
fullDesc _ = mempty -- tabulate . catMaybes . mapParser doc
--  where
--    doc info opt = do
--      guard . not . isEmpty $ n
--      guard . not . isEmpty $ h
--      return (extractChunk n, align . extractChunk $ h <<+>> hdef)
--      where
--        n = optDesc pprefs style info opt
--        h = optHelp opt
--        hdef = Chunk . fmap show_def . optShowDefault $ opt
--        show_def s = parens (string "default:" <+> string s)
--    style = OptDescStyle
--      { descSep = string ","
--      , descHidden = True
--      , descSurround = False }

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
parserHelp :: f a -> ParserHelp
parserHelp p = bodyHelp . vsepChunks $
  [ with_title "Available options:" (fullDesc p)
  , with_title "Available commands:" (cmdDesc p) ]
  where
    with_title :: String -> Chunk Doc -> Chunk Doc
    with_title title = fmap (string title .$.)

-- | Generate option summary.
parserUsage :: HasUsage f => f a -> String -> Doc
parserUsage p progn = hsep
  [ string "Usage:"
  , string progn
  , align (pretty (usage p)) ]

{-# ANN footerHelp "HLint: ignore Eta reduce" #-}
