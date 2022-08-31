{-# LANGUAGE CPP #-}

module Options.Applicative.Help.Pretty
  ( module PP
  , Ann(..)
  , Doc
  , (.$.)
  , groupOrNestLine
  , altSep
  , hangAtIfOver

  , enclose
  , parens
  , brackets
  , hang
  , indent
  , nest

  , text
  , plain
  , deunderline
  , underline
  , debold
  , bold
  , ondullwhite
  , onwhite
  , ondullcyan
  , oncyan
  , ondullmagenta
  , onmagenta
  , ondullblue
  , onblue
  , ondullyellow
  , onyellow
  , ondullgreen
  , ongreen
  , ondullred
  , onred
  , ondullblack
  , onblack
  , dullwhite
  , white
  , dullcyan
  , cyan
  , dullmagenta
  , magenta
  , dullblue
  , blue
  , dullyellow
  , yellow
  , dullgreen
  , green
  , dullred
  , red
  , dullblack
  , black

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
import qualified Options.Applicative.Help.Style as S

import           Prelude

type Doc = PPI.Doc Ann

(.$.) :: Doc -> Doc -> Doc
(.$.) x y = x <> line <> y

-- | Apply the function if we're not at the
--   start of our nesting level.
ifNotAtRoot :: (Doc -> Doc) -> Doc -> Doc
ifNotAtRoot =
  ifElseAtRoot id

-- | Apply the function if we're not at the
--   start of our nesting level.
ifAtRoot :: (Doc -> Doc) -> Doc -> Doc
ifAtRoot =
  flip ifElseAtRoot id

-- | Apply the function if we're not at the
--   start of our nesting level.
ifElseAtRoot :: (Doc -> Doc) -> (Doc -> Doc) -> Doc -> Doc
ifElseAtRoot f g doc =
  PPI.Nesting $ \i ->
    PPI.Column $ \j ->
      if i == j
        then f doc
        else g doc


-- | Render flattened text on this line, or start
--   a new line before rendering any text.
--
--   This will also nest subsequent lines in the
--   group.
groupOrNestLine :: Doc -> Doc
groupOrNestLine =
  PPI.Union
    <$> flatten
    <*> ifNotAtRoot (line <>) . nest 2
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
altSep x y =
  group (x <+> pretty "|" <> line) <> softline' <> y

-- | Printer hacks to get nice indentation for long commands
--   and subcommands.
--
--   If we're starting this section over the desired width
--   (usually 1/3 of the ribbon), then we will make a line
--   break, indent all of the usage, and go.
--
--   The ifAtRoot is an interesting clause. If this whole
--   operation is put under a `group` then the linebreak
--   will disappear; then item d will therefore not be at
--   the starting column, and it won't be indented more.
hangAtIfOver :: Int -> Int -> Doc -> Doc
hangAtIfOver i j d =
  PPI.Column $ \k ->
    if k <= j then
      align d
    else
      linebreak <> ifAtRoot (indent i) d

(</>) :: Doc -> Doc -> Doc
(</>) x y = x <> softline <> y

(<$$>) :: Doc -> Doc -> Doc
(<$$>) x y = x <> linebreak <> y

(<//>) :: Doc -> Doc -> Doc
(<//>) x y = x <> softbreak <> y

linebreak :: Doc
linebreak = flatAlt line mempty

softbreak :: Doc
softbreak = group linebreak

-- | Traced version of 'PP.string'.
string :: String -> Doc
string = PP.pretty

-- | Traced version of 'PP.parens'.
parens :: Doc -> Doc
parens = PP.parens

-- | Traced version of 'PP.brackets'.
brackets :: Doc -> Doc
brackets = PP.brackets

-- | Traced version of 'PP.enclose'.
enclose
    :: Doc -- ^ L
    -> Doc -- ^ R
    -> Doc -- ^ x
    -> Doc -- ^ LxR
enclose = PP.enclose

-- | Traced version of 'PP.hang'.
hang :: Int -> Doc -> Doc
hang = PP.hang

-- | Traced version of 'PP.nest'.
nest :: Int -> Doc -> Doc
nest = PP.nest

-- | Traced version of 'PP.indent'.
indent :: Int -> Doc -> Doc
indent = PP.indent

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

text :: String -> Doc
text = pretty

plain :: Doc -> Doc
plain = id

deunderline :: Doc -> Doc
deunderline = id

underline :: Doc -> Doc
underline = annotate (AnnStyle S.underlined)

debold :: Doc -> Doc
debold = id

bold :: Doc -> Doc
bold = annotate (AnnStyle S.bold)

ondullwhite :: Doc -> Doc
ondullwhite = annotate (AnnStyle (S.bgColorDull S.White))

onwhite :: Doc -> Doc
onwhite = annotate (AnnStyle (S.bgColor S.White))

ondullcyan :: Doc -> Doc
ondullcyan = annotate (AnnStyle (S.bgColorDull S.Cyan))

oncyan :: Doc -> Doc
oncyan = annotate (AnnStyle (S.bgColor S.Cyan))

ondullmagenta :: Doc -> Doc
ondullmagenta = annotate (AnnStyle (S.bgColorDull S.Magenta))

onmagenta :: Doc -> Doc
onmagenta = annotate (AnnStyle (S.bgColor S.Magenta))

ondullblue :: Doc -> Doc
ondullblue = annotate (AnnStyle (S.bgColorDull S.Blue))

onblue :: Doc -> Doc
onblue = annotate (AnnStyle (S.bgColor S.Blue))

ondullyellow :: Doc -> Doc
ondullyellow = annotate (AnnStyle (S.bgColorDull S.Yellow))

onyellow :: Doc -> Doc
onyellow = annotate (AnnStyle (S.bgColor S.Yellow))

ondullgreen :: Doc -> Doc
ondullgreen = annotate (AnnStyle (S.bgColorDull S.Green))

ongreen :: Doc -> Doc
ongreen = annotate (AnnStyle (S.bgColor S.Green))

ondullred :: Doc -> Doc
ondullred = annotate (AnnStyle (S.bgColorDull S.Red))

onred :: Doc -> Doc
onred = annotate (AnnStyle (S.bgColor S.Red))

ondullblack :: Doc -> Doc
ondullblack = annotate (AnnStyle (S.bgColorDull S.Black))

onblack :: Doc -> Doc
onblack = annotate (AnnStyle (S.bgColor S.Black))

dullwhite :: Doc -> Doc
dullwhite = annotate (AnnStyle (S.colorDull S.White))

white :: Doc -> Doc
white = annotate (AnnStyle (S.color S.White))

dullcyan :: Doc -> Doc
dullcyan = annotate (AnnStyle (S.colorDull S.Cyan))

cyan :: Doc -> Doc
cyan = annotate (AnnStyle (S.color S.Cyan))

dullmagenta :: Doc -> Doc
dullmagenta = annotate (AnnStyle (S.colorDull S.Magenta))

magenta :: Doc -> Doc
magenta = annotate (AnnStyle (S.color S.Magenta))

dullblue :: Doc -> Doc
dullblue = annotate (AnnStyle (S.colorDull S.Blue))

blue :: Doc -> Doc
blue = annotate (AnnStyle (S.color S.Blue))

dullyellow :: Doc -> Doc
dullyellow = annotate (AnnStyle (S.colorDull S.Yellow))

yellow :: Doc -> Doc
yellow = annotate (AnnStyle (S.color S.Yellow))

dullgreen :: Doc -> Doc
dullgreen = annotate (AnnStyle (S.colorDull S.Green))

green :: Doc -> Doc
green = annotate (AnnStyle (S.color S.Green))

dullred :: Doc -> Doc
dullred = annotate (AnnStyle (S.colorDull S.Red))

red :: Doc -> Doc
red = annotate (AnnStyle (S.color S.Red))

dullblack :: Doc -> Doc
dullblack = annotate (AnnStyle (S.colorDull S.Black))

black :: Doc -> Doc
black = annotate (AnnStyle (S.color S.Black))
