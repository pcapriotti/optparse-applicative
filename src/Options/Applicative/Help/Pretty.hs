{-# LANGUAGE CPP #-}

module Options.Applicative.Help.Pretty
  ( module PP
  , (.$.)
  , groupOrNestLine
  , altSep
  , Ann(..)
  , Doc

  , enclose
  , parens
  , brackets
  , hang
  , indent
  , nest

  -- TODO Remove these
  -- , (<$>)
  , (</>)
  , (<$$>)
  , (<//>)
  , string

  , isEffectivelyEmpty

  , renderShowS
  ) where

import           Control.Applicative
#if !MIN_VERSION_base(4,11,0)
import           Data.Semigroup ((<>))
#endif

import           Options.Applicative.Help.Ann
import           Prettyprinter hiding ((<>), Doc, enclose, parens, brackets, hang, indent, nest)
import qualified Prettyprinter as PP
import qualified Prettyprinter.Internal as PPI
import           Prettyprinter.Render.String (renderShowS)

import           Prelude

type Doc = PPI.Doc Ann

(.$.) :: Doc -> Doc -> Doc
(.$.) x y = annTrace 1 "(.$.)" (x <> line <> y)

-- | Apply the function if we're not at the
--   start of our nesting level.
ifNotAtRoot :: (Doc -> Doc) -> Doc -> Doc
ifNotAtRoot f doc = annTrace 1 "ifNotAtRoot" $
  PPI.Nesting $ \i ->
    PPI.Column $ \j ->
      if i == j
        then doc
        else f doc


-- | Render flattened text on this line, or start
--   a new line before rendering any text.
--
--   This will also nest subsequent lines in the
--   group.
groupOrNestLine :: Doc -> Doc
groupOrNestLine d = annTrace 1 "groupOrNestLine" $
  (PPI.Union
    <$> flatten
    <*> ifNotAtRoot (line <>)) d
  where flatten :: Doc -> Doc
        flatten doc = case doc of
          PPI.FlatAlt _ y     -> flatten y
          PPI.Cat x y         -> PPI.Cat (flatten x) (flatten y)
          PPI.Nest i x        -> PPI.Nest i (flatten x)
          PPI.Line            -> PPI.Fail
          PPI.Union x _       -> flatten x
          PPI.Column f        -> PPI.Column (flatten . f)
          PPI.WithPageWidth f -> PPI.WithPageWidth (flatten . f)
          PPI.Nesting f       -> PPI.Nesting (flatten . f)
          PPI.Annotated ann x -> PPI.Annotated ann (flatten x)

          x@PPI.Fail   -> x
          x@PPI.Empty  -> x
          x@PPI.Char{} -> x
          x@PPI.Text{} -> x

-- | Separate items in an alternative with a pipe.
--
--   If the first document and the pipe don't fit
--   on the line, then mandatorily flow the next entry
--   onto the following line.
--
--   The (<//>) softbreak ensures that if the document
--   does fit on the line, there is at least a space,
--   but it's possible for y to still appear on the
--   next line.
altSep :: Doc -> Doc -> Doc
altSep x y = annTrace 1 "altSep" $
  group (x <+> pretty "|" <> line) <> softline' <> y


-- (<$>) :: Doc -> Doc -> Doc
-- (<$>) = \x y -> x <> line <> y

(</>) :: Doc -> Doc -> Doc
(</>) x y = annTrace 1 "(</>)" $ x <> softline <> y

(<$$>) :: Doc -> Doc -> Doc
(<$$>) x y = annTrace 1 "(<$$>)" $x <> linebreak <> y

(<//>) :: Doc -> Doc -> Doc
(<//>) x y = annTrace 1 "(<//>)" $ x <> softbreak <> y

linebreak :: Doc
linebreak = annTrace 0 "linebreak" $ flatAlt line mempty

softbreak :: Doc
softbreak = annTrace 0 "softbreak" $ group linebreak

-- | Traced version of 'PP.string'.
string :: String -> Doc
string = annTrace 0 "string" . PP.pretty

-- | Traced version of 'PP.parens'.
parens :: Doc -> Doc
parens = annTrace 1 "parens" . PP.parens

-- | Traced version of 'PP.brackets'.
brackets :: Doc -> Doc
brackets = annTrace 1 "brackets" . PP.brackets

-- | Traced version of 'PP.enclose'.
enclose
    :: Doc -- ^ L
    -> Doc -- ^ R
    -> Doc -- ^ x
    -> Doc -- ^ LxR
enclose l r x = annTrace 1 "enclose" (PP.enclose l r x)

-- | Traced version of 'PP.hang'.
hang :: Int -> Doc -> Doc
hang n = annTrace 1 "hang" . PP.hang n

-- | Traced version of 'PP.nest'.
nest :: Int -> Doc -> Doc
nest n = annTrace 1 "nest" . PP.nest n

-- | Traced version of 'PP.indent'.
indent :: Int -> Doc -> Doc
indent n = annTrace 1 "indent" . PP.indent n

-- | Determine if the document is empty when rendered
isEffectivelyEmpty :: Doc -> Bool
isEffectivelyEmpty doc = case doc of
  PPI.Fail -> True
  PPI.Empty -> True
  PPI.Char _ -> False
  PPI.Text _ _ -> False
  PPI.Line -> False
  PPI.FlatAlt _ d -> isEffectivelyEmpty d
  PPI.Cat a b -> isEffectivelyEmpty a && isEffectivelyEmpty b
  PPI.Nest _ d -> isEffectivelyEmpty d
  PPI.Union _ d -> isEffectivelyEmpty d
  PPI.Column _ -> True
  PPI.WithPageWidth _ -> False
  PPI.Nesting _ -> False
  PPI.Annotated _ d -> isEffectivelyEmpty d
