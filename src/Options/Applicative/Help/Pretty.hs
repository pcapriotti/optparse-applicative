{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Options.Applicative.Help.Pretty
  ( module Prettyprinter
  , (.$.)
  , groupOrNestLine
  , altSep
  ) where

import           Control.Applicative
#if !MIN_VERSION_base(4,11,0)
import           Data.Semigroup ((<>))
#endif

import           Prettyprinter hiding ((<$>), (<>), columns)
import           Prettyprinter.Internal (Doc (..))
import qualified Prettyprinter as PP
import           Prelude

(.$.) :: Doc ann -> Doc ann -> Doc ann
(.$.) x y = x <> line <> y


-- | Apply the function if we're not at the
--   start of our nesting level.
ifNotAtRoot :: (Doc ann -> Doc ann) -> Doc ann -> Doc ann
ifNotAtRoot f doc =
  Nesting $ \i ->
    Column $ \j ->
      if i == j
        then doc
        else f doc


-- | Render flattened text on this line, or start
--   a new line before rendering any text.
--
--   This will also nest subsequent lines in the
--   group.
groupOrNestLine :: Doc ann -> Doc ann
groupOrNestLine =
  Union
    <$> flatten
    <*> ifNotAtRoot (line <>) . nest 2
  where flatten :: Doc ann -> Doc ann
        flatten doc = case doc of
          FlatAlt _ y     -> flatten y
          Cat x y         -> Cat (flatten x) (flatten y)
          Nest i x        -> Nest i (flatten x)
          Line            -> Fail
          Union x _       -> flatten x
          Column f        -> Column (flatten . f)
          WithPageWidth f -> WithPageWidth (flatten . f)
          Nesting f       -> Nesting (flatten . f)
          Annotated ann x -> Annotated ann (flatten x)

          x@Fail   -> x
          x@Empty  -> x
          x@Char{} -> x
          x@Text{} -> x



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
altSep :: Doc ann -> Doc ann -> Doc ann
altSep x y =
  group (x <+> "|" <> line) <> softline' <> y
