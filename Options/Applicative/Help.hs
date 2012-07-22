{-# LANGUAGE PatternGuards #-}
module Options.Applicative.Help (
  cmdDesc,
  briefDesc,
  fullDesc,
  parserHelpText,
  ) where

import Data.Lens.Common
import Data.List
import Data.Maybe
import Options.Applicative.Common
import Options.Applicative.Types
import Options.Applicative.Utils

showOption :: OptName -> String
showOption (OptLong n) = "--" ++ n
showOption (OptShort n) = '-' : [n]

-- | Style for rendering an option.
data OptDescStyle = OptDescStyle
  { descSep :: String
  , descHidden :: Bool
  , descSurround :: Bool }

-- | Generate description for a single option.
optDesc :: ParserPrefs -> OptDescStyle -> OptHelpInfo -> Option a -> String
optDesc pprefs style info opt =
  let ns = optionNames $ opt^.optMain
      mv = opt^.optMetaVar
      descs = map showOption (sort ns)
      desc' = intercalate (descSep style) descs <+> mv
      show_opt
        | opt^.optVisibility == Hidden
        = descHidden style
        | otherwise
        = opt^.optVisibility == Visible
      suffix
        | hinfoMulti info
        = pprefs^.prefMultiSuffix
        | otherwise
        = ""
      render text
        | not show_opt
        = ""
        | null text || not (descSurround style)
        = text ++ suffix
        | hinfoDefault info
        = "[" ++ text ++ "]" ++ suffix
        | null (drop 1 descs)
        = text ++ suffix
        | otherwise
        = "(" ++ text ++ ")" ++ suffix
  in render desc'

-- | Generate descriptions for commands.
cmdDesc :: Parser a -> [String]
cmdDesc = concat . mapParser desc
  where
    desc _ opt
      | CmdReader cmds p <- opt^.optMain
      = tabulate [(cmd, d)
                 | cmd <- cmds
                 , d <- maybeToList . fmap (getL infoProgDesc) $ p cmd ]
      | otherwise
      = []

-- | Generate a brief help text for a parser.
briefDesc :: ParserPrefs -> Parser a -> String
briefDesc pprefs = foldr (<+>) "" . mapParser (optDesc pprefs style)
  where
    style = OptDescStyle
      { descSep = "|"
      , descHidden = False
      , descSurround = True }

-- | Generate a full help text for a parser.
fullDesc :: ParserPrefs -> Parser a -> [String]
fullDesc pprefs = tabulate . catMaybes . mapParser doc
  where
    doc info opt
      | null n = Nothing
      | null h = Nothing
      | otherwise = Just (n, h)
      where n = optDesc pprefs style info opt
            h = opt^.optHelp
    style = OptDescStyle
      { descSep = ","
      , descHidden = True
      , descSurround = False }

-- | Generate the help text for a program.
parserHelpText :: ParserPrefs -> ParserInfo a -> String
parserHelpText pprefs pinfo = unlines
   $ nn [pinfo^.infoHeader]
  ++ [ "  " ++ line | line <- nn [pinfo^.infoProgDesc] ]
  ++ [ line | let opts = fullDesc pprefs p
            , not (null opts)
            , line <- ["", "Common options:"] ++ opts
            , pinfo^.infoFullDesc ]
  ++ [ line | let cmds = cmdDesc p
            , not (null cmds)
            , line <- ["", "Available commands:"] ++ cmds
            , pinfo^.infoFullDesc ]
  ++ [ line | footer <- nn [pinfo^.infoFooter]
            , line <- ["", footer] ]
  where
    nn = filter (not . null)
    p = pinfo^.infoParser
