{-# LANGUAGE PackageImports #-}
module Options.Applicative.Help.Types (
    ParserHelp (..)
  , renderHelp
  ) where

import Data.Semigroup
import Prelude

import Options.Applicative.Help.Chunk
import Options.Applicative.Help.Pretty
import qualified Data.Text.Lazy as Lazy

data ParserHelp = ParserHelp
  { helpError :: Chunk Doc
  , helpSuggestions :: Chunk Doc
  , helpHeader :: Chunk Doc
  , helpUsage :: Chunk Doc
  , helpDescription :: Chunk Doc
  , helpBody :: Chunk Doc
  , helpGlobals :: Chunk Doc
  , helpFooter :: Chunk Doc
  }

instance Show ParserHelp where
  showsPrec _ h = shows (renderHelp 80 h)

instance Monoid ParserHelp where
  mempty = ParserHelp mempty mempty mempty mempty mempty mempty mempty mempty
  mappend = (<>)

instance Semigroup ParserHelp where
  (ParserHelp e1 s1 h1 u1 d1 b1 g1 f1) <> (ParserHelp e2 s2 h2 u2 d2 b2 g2 f2)
    = ParserHelp (mappend e1 e2) (mappend s1 s2)
                 (mappend h1 h2) (mappend u1 u2)
                 (mappend d1 d2) (mappend b1 b2)
                 (mappend g1 g2) (mappend f1 f2)

helpText :: ParserHelp -> Doc
helpText (ParserHelp e s h u d b g f) =
  extractChunk $
    vsepChunks [e, s, h, u, fmap (indent 2) d, b, g, f]

-- | Convert a help text to 'Text'.
renderHelp :: Int -> ParserHelp -> Lazy.Text
renderHelp cols
  = prettyLazyText 1.0 cols
  . helpText
