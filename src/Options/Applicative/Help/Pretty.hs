{-# LANGUAGE CPP #-}
module Options.Applicative.Help.Pretty
  ( module Prettyprinter
  , module Prettyprinter.Render.Terminal
  , Doc
  , SimpleDoc

  , (.$.)
  , (</>)

  , groupOrNestLine
  , altSep
  , hangAtIfOver

  , prettyString
  ) where

#if !MIN_VERSION_base(4,11,0)
import           Data.Semigroup ((<>), mempty)
#endif
import qualified Data.Text.Lazy as Lazy

import           Prettyprinter hiding (Doc)
import qualified Prettyprinter as PP
import           Prettyprinter.Render.Terminal

import           Prelude

type Doc = PP.Doc AnsiStyle
type SimpleDoc = SimpleDocStream AnsiStyle

linebreak :: Doc
linebreak = flatAlt line mempty

(.$.) :: Doc -> Doc -> Doc
x .$. y = x <> line <> y
(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y

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
  nesting $ \i ->
    column $ \j ->
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
  group . ifNotAtRoot (linebreak <>) . nest 2


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
  group (x <+> pretty '|' <> line) <> group linebreak <>  y


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
  column $ \k ->
    if k <= j then
      align d
    else
      linebreak <> ifAtRoot (indent i) d


renderPretty :: Double -> Int -> Doc -> SimpleDocStream AnsiStyle
renderPretty ribbonFraction lineWidth
  = layoutPretty LayoutOptions
      { layoutPageWidth = AvailablePerLine lineWidth ribbonFraction }

prettyString :: Double -> Int -> Doc -> String
prettyString ribbonFraction lineWidth
  = streamToString
  . renderPretty ribbonFraction lineWidth

streamToString :: SimpleDocStream AnsiStyle -> String
streamToString sdoc =
  let
    rendered =
      Prettyprinter.Render.Terminal.renderLazy sdoc
  in
    Lazy.unpack rendered
