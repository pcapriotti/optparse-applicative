{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Options.Applicative.Usage
  ( Usage0(..)
  , prettyUsage0
  , Usage(..)
  , prettyUsage
  ) where

import Control.Applicative
import Data.Bimonoid
import Data.Bimonoid.Expr
import Data.List
import Data.Monoid

import Options.Applicative.Help.Pretty

newtype Usage0 a = Usage0
  { unUsage0 :: Expr a }
  deriving (Functor, Applicative, Monad, Monoid, Bimonoid)

prettyExpr :: (a -> Doc) -> Expr a -> Doc
prettyExpr p (Lit x) = p x
prettyExpr p (ProdExpr ts) = fillSep (map (prettyExpr' p) ts)

prettyExpr' :: (a -> Doc) -> Expr' a -> Doc
prettyExpr' p (Lit' x) = p x
prettyExpr' _ (SumExpr []) = mempty
prettyExpr' p (SumExpr ts) = parens (foldl1' op (map (prettyExpr p) ts))
  where
    op doc1 doc2 = doc1 </> char '|' </> doc2

prettyUsage0 :: (a -> Doc) -> Usage0 a -> Doc
prettyUsage0 p = prettyExpr p . unUsage0

instance Pretty a => Pretty (Usage0 a) where
  pretty = prettyUsage0 pretty

newtype Usage a = Usage
  { unUsage :: StarExpr a }
  deriving (Functor, Applicative, Monad, Monoid, Bimonoid, StarBimonoid)

prettyStarExpr :: (a -> Doc) -> (Doc -> Doc) -> StarExpr a -> Doc
prettyStarExpr p pstar = go
  where
    go = prettyExpr (either (pstar . go) p) . unStarExpr

prettyUsage :: (a -> Doc) -> (Doc -> Doc) -> Usage a -> Doc
prettyUsage p pstar = prettyStarExpr p pstar . unUsage

instance Pretty a => Pretty (Usage a) where
  pretty = prettyUsage pretty (<> char '*')
