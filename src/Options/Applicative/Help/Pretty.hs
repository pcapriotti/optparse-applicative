{-# LANGUAGE CPP #-}
module Options.Applicative.Help.Pretty
  ( module Prettyprinter
  , module Prettyprinter.Render.Terminal
  , AnsiDoc

  , (.$.)
  , (</>)

  , groupOrNestLine
  , altSep
  , hangAtIfOver

  , ansiDocToPrettyString
  ) where

#if !MIN_VERSION_base(4,11,0)
import           Data.Semigroup ((<>), mempty)
#endif
import qualified Data.Text.Lazy as Lazy

import           Prettyprinter
import           Prettyprinter.Render.Terminal

import           Prelude

-- TODO: Pass semantically more meaningful type to Doc, as annotation (as parameter).
--   From docs:
--     Summary: Use semantic annotations for Doc, and after layouting map to backend-specific ones.
--     For example, suppose you want to prettyprint some programming language code. If you want
--     keywords to be red, you should annotate the Doc with a type that has a Keyword field (without
--     any notion of color), and then after layouting convert the annotations to map Keyword to e.g.
--     Red (using reAnnotateS). The alternative that I do not recommend is directly annotating the
--     Doc with Red.
-- Btw I put this comment here for no good reason, it is a general comment for the whole refactoring.

linebreak :: Doc a
linebreak = flatAlt line mempty

(.$.) :: Doc a -> Doc a -> Doc a
x .$. y = x <> line <> y
(</>) :: Doc a -> Doc a -> Doc a
x </> y = x <> softline <> y

-- | Apply the function if we're not at the
--   start of our nesting level.
ifNotAtRoot :: (Doc a -> Doc a) -> Doc a -> Doc a
ifNotAtRoot =
  ifElseAtRoot id

-- | Apply the function if we're not at the
--   start of our nesting level.
ifAtRoot :: (Doc a -> Doc a) -> Doc a -> Doc a
ifAtRoot =
  flip ifElseAtRoot id

-- | Apply the function if we're not at the
--   start of our nesting level.
ifElseAtRoot :: (Doc a -> Doc a) -> (Doc a -> Doc a) -> Doc a -> Doc a
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
groupOrNestLine :: Doc a -> Doc a
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
altSep :: Doc a -> Doc a -> Doc a
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
hangAtIfOver :: Int -> Int -> Doc a -> Doc a
hangAtIfOver i j d =
  column $ \k ->
    if k <= j then
      align d
    else
      linebreak <> ifAtRoot (indent i) d


renderPretty :: Double -> Int -> Doc a -> SimpleDocStream a
renderPretty ribbonFraction lineWidth
  = layoutPretty LayoutOptions
      { layoutPageWidth = AvailablePerLine lineWidth ribbonFraction }

-- TODO: All the functions above are for any Doc a, only functions / types below are specific to
-- AnsiDoc. So look into splitting them into different modules most likely.

type AnsiDoc = Doc AnsiStyle

ansiDocToPrettyString :: Double -> Int -> AnsiDoc -> String
ansiDocToPrettyString ribbonFraction lineWidth =
  ansiStreamToString
    . renderPretty ribbonFraction lineWidth

ansiStreamToString :: SimpleDocStream AnsiStyle -> String
ansiStreamToString sdoc =
  let rendered =
        Prettyprinter.Render.Terminal.renderLazy sdoc
   in Lazy.unpack rendered
