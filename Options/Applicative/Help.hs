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
optDesc :: OptDescStyle -> Option r a -> String
optDesc style opt =
  let ns = optionNames $ opt^.optMain
      mv = opt^.optMetaVar
      descs = map showOption (sort ns)
      desc' = intercalate (descSep style) descs <+> mv
      render text
        | not (opt^.optShow) && not (descHidden style)
        = ""
        | null text || not (descSurround style)
        = text
        | isJust (opt^.optDefault)
        = "[" ++ text ++ "]"
        | null (drop 1 descs)
        = text
        | otherwise
        = "(" ++ text ++ ")"
  in render desc'

-- | Generate descriptions for commands.
cmdDesc :: Parser a -> String
cmdDesc = intercalate "\n"
        . filter (not . null)
        . mapParser desc
  where
    desc opt
      | CmdReader cmds p <- opt^.optMain
      = tabulate [(cmd, d)
                 | cmd <- cmds
                 , d <- maybeToList . fmap (getL infoProgDesc) $ p cmd ]
      | otherwise
      = ""

-- | Generate a brief help text for a parser.
briefDesc :: Parser a -> String
briefDesc = foldr (<+>) "" . mapParser (optDesc style)
  where
    style = OptDescStyle
      { descSep = "|"
      , descHidden = False
      , descSurround = True }

-- | Generate a full help text for a parser.
fullDesc :: Parser a -> String
fullDesc = tabulate . catMaybes . mapParser doc
  where
    doc opt
      | null n = Nothing
      | null h = Nothing
      | otherwise = Just (n, h)
      where n = optDesc style opt
            h = opt^.optHelp
    style = OptDescStyle
      { descSep = ","
      , descHidden = True
      , descSurround = False }

-- | Generate the help text for a program.
parserHelpText :: ParserInfo a -> String
parserHelpText pinfo = unlines
   $ nn [pinfo^.infoHeader]
  ++ [ "  " ++ line | line <- nn [pinfo^.infoProgDesc] ]
  ++ [ line | desc <- nn [fullDesc p]
            , line <- ["", "Common options:", desc]
            , pinfo^.infoFullDesc ]
  ++ [ line | desc <- nn [cmdDesc p]
            , line <- ["", "Available commands:", desc]
            , pinfo^.infoFullDesc ]
  ++ [ line | footer <- nn [pinfo^.infoFooter]
            , line <- ["", footer] ]
  where
    nn = filter (not . null)
    p = pinfo^.infoParser
