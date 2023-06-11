module Options.Applicative.Help.Types (
    ParserHelp (..)
  , renderHelp
  ) where

import Data.Semigroup
import Prelude

import Options.Applicative.Help.Chunk
import Options.Applicative.Help.Pretty
import Options.Applicative.Help.HelpDoc (HelpDoc, helpDocToAnsiDoc)

data ParserHelp = ParserHelp
  { helpError :: Chunk HelpDoc
  , helpSuggestions :: Chunk HelpDoc
  , helpHeader :: Chunk HelpDoc
  , helpUsage :: Chunk HelpDoc
  , helpDescription :: Chunk HelpDoc
  , helpBody :: Chunk HelpDoc
  , helpGlobals :: Chunk HelpDoc
  , helpFooter :: Chunk HelpDoc
  }

instance Show ParserHelp where
  showsPrec _ h = showString (renderHelp 80 h)

instance Monoid ParserHelp where
  mempty = ParserHelp mempty mempty mempty mempty mempty mempty mempty mempty
  mappend = (<>)

instance Semigroup ParserHelp where
  (ParserHelp e1 s1 h1 u1 d1 b1 g1 f1) <> (ParserHelp e2 s2 h2 u2 d2 b2 g2 f2)
    = ParserHelp (mappend e1 e2) (mappend s1 s2)
                 (mappend h1 h2) (mappend u1 u2)
                 (mappend d1 d2) (mappend b1 b2)
                 (mappend g1 g2) (mappend f1 f2)

helpText :: ParserHelp -> HelpDoc
helpText (ParserHelp e s h u d b g f) =
  extractChunk $
    vsepChunks [e, s, h, u, fmap (indent 2) d, b, g, f]

-- TODO: Probably should rename this module to ParserHelp or smth like that?

-- | Convert a help text to 'String'.
renderHelp :: Int -> ParserHelp -> String
renderHelp cols
  = ansiDocToPrettyString 1.0 cols
  . helpDocToAnsiDoc
  . helpText
