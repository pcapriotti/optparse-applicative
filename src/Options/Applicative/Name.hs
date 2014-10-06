module Options.Applicative.Name where

import Options.Applicative.Help.Pretty

data Name
  = LongName String
  | ShortName Char
  deriving (Eq, Read, Show)

type Names = Name

instance Pretty Name where
  pretty (LongName n) = string $ '-' : '-' : n
  pretty (ShortName c) = string $ '-' : [c]
