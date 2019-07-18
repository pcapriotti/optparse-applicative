module Options.Applicative.Help.Pretty
  ( module Text.PrettyPrint.ANSI.Leijen
  , (.$.)
  , groupOrNestLine
  ) where

import           Control.Applicative
import           Data.Monoid (mappend)

import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (<>), columns)
import           Text.PrettyPrint.ANSI.Leijen.Internal (Doc (..), flatten)
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import           Prelude

(.$.) :: Doc -> Doc -> Doc
(.$.) = (PP.<$>)


-- | Apply the function if we're not at the
--   start of our nesting level.
ifNotAtRoot :: (Doc -> Doc) -> Doc -> Doc
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
groupOrNestLine :: Doc -> Doc
groupOrNestLine =
  Union
    <$> flatten
    <*> ifNotAtRoot (mappend line) . nest 2
