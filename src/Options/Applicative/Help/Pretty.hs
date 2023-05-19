{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
module Options.Applicative.Help.Pretty
  ( module Text.PrettyPrint.ANSI.Leijen
  , Doc
  , indent
  , renderPretty
  , displayS
  , (.$.)
  , groupOrNestLine
  , altSep
  , hangAtIfOver
  ) where

#if !MIN_VERSION_base(4,11,0)
import           Data.Semigroup ((<>))
#endif

import           Text.PrettyPrint.ANSI.Leijen hiding (Doc, (<$>), (<>), columns, indent, renderPretty, displayS)
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import           Prelude

type Doc = PP.Doc

indent :: Int -> PP.Doc -> PP.Doc
indent = PP.indent

renderPretty :: Float -> Int -> PP.Doc -> SimpleDoc
renderPretty = PP.renderPretty

displayS :: SimpleDoc -> ShowS
displayS = PP.displayS

(.$.) :: Doc -> Doc -> Doc
(.$.) = (PP.<$>)


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
  group (x <+> char '|' <> line) <//> y


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
