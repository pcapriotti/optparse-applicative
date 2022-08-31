{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Options.Applicative.Help.Types (
    ParserHelp (..)
  , renderHelp
  , helpText
  ) where

import Data.Semigroup
import Data.String (fromString)
import Options.Applicative.Help.Chunk
import Options.Applicative.Help.Pretty
import Options.Applicative.Help.Style (SetStyle (..), styleToRawText, defaultStyle)
import Prelude

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

helpText :: ParserHelp -> Doc
helpText (ParserHelp e s h u d b g f) =
  extractChunk $
    vsepChunks [e, s, h, u, fmap (indent 2) d, b, g, f]

-- | Convert a help text to 'String'.
renderHelp :: Int -> ParserHelp -> String
renderHelp cols
  =  (`renderShowS` "")
  . renderAnsi
  . layoutPretty (LayoutOptions (AvailablePerLine cols 1.0))
  . helpText

renderAnsi :: SimpleDocStream Ann -> SimpleDocStream ()
renderAnsi
  = renderCtxDecorated defaultStyle renderPush renderPop
  . alterAnnotationsS alter
  where
    alter :: Ann -> Maybe SetStyle
    alter (AnnStyle setStyle) = Just setStyle
    renderPush :: SetStyle -> SetStyle -> SimpleDocStream () -> SimpleDocStream ()
    renderPush _ setStyle = SText 0 (fromString (styleToRawText setStyle))
    renderPop :: SetStyle -> SetStyle -> SimpleDocStream () -> SimpleDocStream ()
    renderPop setStyle _ = SText 0 (fromString (styleToRawText setStyle))

renderCtxDecorated
    :: forall ann.
       Semigroup ann
    => ann
    -> (ann -> ann -> SimpleDocStream () -> SimpleDocStream ())  -- ^ How to render an annotation
    -> (ann -> ann -> SimpleDocStream () -> SimpleDocStream ())  -- ^ How to render the removed annotation
    -> SimpleDocStream ann
    -> SimpleDocStream ()
renderCtxDecorated topAnn push pop = go [topAnn]
  where
    go :: [ann] -> SimpleDocStream ann -> SimpleDocStream ()
    go _                      SFail               = SFail
    go []                     SEmpty              = SEmpty
    go (_:_:_)                SEmpty              = SEmpty
    go (_:_)                  SEmpty              = SEmpty
    go stack                  (SChar c rest)      = SChar c (go stack rest)
    go stack                  (SText l t rest)    = SText l t (go stack rest)
    go stack                  (SLine i rest)      = SLine i (go stack rest)
    go stack@(ctxAnn:_)       (SAnnPush ann rest) = push ctxAnn ann (go ((ctxAnn <> ann) : stack) rest)
    go (ann:stack@(ctxAnn:_)) (SAnnPop rest)      = pop ctxAnn ann (go stack rest)
    go _                      (SAnnPush _ _)      = error "An unpaired style initiator was encountered. This is a bug in the layout algorithm.  Please report this as a bug"
    go _                      (SAnnPop _)         = error "An unpaired style terminator was encountered. This is a bug in the layout algorithm.  Please report this as a bug"
{-# INLINE renderCtxDecorated #-}
