module Options.Applicative.Utils (
  (<+>),
  vcat,
  tabulate,
  pad
  ) where

import Data.List (intercalate)

-- | Concatenate two strings with a space in the middle.
(<+>) :: String -> String -> String
"" <+> s = s
s <+> "" = s
s1 <+> s2 = s1 ++ " " ++ s2

-- | Concatenate strings vertically with empty lines in between.
vcat :: [String] -> String
vcat = intercalate "\n\n" . filter (not . null)

tabulate' :: Int -> [(String, String)] -> [String]
tabulate' size table =
  [ "  " ++ pad size key ++ " " ++ value
  | (key, value) <- table ]

-- | Display pairs of strings in a table.
tabulate :: [(String, String)] -> [String]
tabulate = tabulate' 24

-- | Pad a string to a fixed size with whitespace.
pad :: Int -> String -> String
pad size str = str ++ replicate (size - n `max` 0) ' '
  where n = length str
