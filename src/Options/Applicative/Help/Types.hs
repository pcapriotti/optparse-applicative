module Options.Applicative.Help.Types (
    ParserHelp (..)
  , renderHelp
  ) where

import Data.Semigroup
import Prelude

import Options.Applicative.Help.Chunk
import Options.Applicative.Help.Pretty
import Prettyprinter.Render.String

data ParserHelp ann = ParserHelp
  { helpError :: Chunk (Doc ann)
  , helpSuggestions :: Chunk (Doc ann)
  , helpHeader :: Chunk (Doc ann)
  , helpUsage :: Chunk (Doc ann)
  , helpDescription :: Chunk (Doc ann)
  , helpBody :: Chunk (Doc ann)
  , helpGlobals :: Chunk (Doc ann)
  , helpFooter :: Chunk (Doc ann)
  }

instance Show (ParserHelp ann) where
  showsPrec _ h = showString (renderHelp 80 h)

instance Monoid (ParserHelp ann) where
  mempty = ParserHelp mempty mempty mempty mempty mempty mempty mempty mempty
  mappend = (<>)

instance Semigroup (ParserHelp ann) where
  (ParserHelp e1 s1 h1 u1 d1 b1 g1 f1) <> (ParserHelp e2 s2 h2 u2 d2 b2 g2 f2)
    = ParserHelp (mappend e1 e2) (mappend s1 s2)
                 (mappend h1 h2) (mappend u1 u2)
                 (mappend d1 d2) (mappend b1 b2)
                 (mappend g1 g2) (mappend f1 f2)

helpText :: ParserHelp ann -> Doc ann
helpText (ParserHelp e s h u d b g f) =
  extractChunk $
    vsepChunks [e, s, h, u, fmap (indent 2) d, b, g, f]

-- | Convert a help text to 'String'.
renderHelp :: Int -> ParserHelp ann -> String
renderHelp cols
  = (`renderShowS` "")
  . layoutPretty (LayoutOptions (AvailablePerLine cols 1.0))
  . helpText
