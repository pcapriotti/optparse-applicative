{-# LANGUAGE FlexibleInstances #-}

module Options.Applicative.Help.Ann
  ( Ann(..)
  ) where

import Options.Applicative.Help.Style

newtype Ann = AnnStyle SetStyle
  deriving (Eq, Show)
