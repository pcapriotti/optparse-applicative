module Options.Applicative.Help.Pretty
  ( module Text.PrettyPrint.ANSI.Leijen
  , (.$.)
  ) where

import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (<>), columns)
import qualified Text.PrettyPrint.ANSI.Leijen as PP

(.$.) :: Doc -> Doc -> Doc
(.$.) = (PP.<$>)
