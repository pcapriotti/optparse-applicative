module Options.Applicative.Utils (
  (<+>),
  vcat,
  tabulate,
  pad
  ) where

import Data.List

(<+>) :: String -> String -> String
"" <+> s = s
s <+> "" = s
s1 <+> s2 = s1 ++ " " ++ s2

vcat :: [String] -> String
vcat = intercalate "\n\n" . filter (not . null)

tabulate' :: Int -> [(String, String)] -> String
tabulate' size table = unlines
  [ "  " ++ pad size key ++ " " ++ value
  | (key, value) <- table ]

tabulate :: [(String, String)] -> String
tabulate = tabulate' 24

pad :: Int -> String -> String
pad size str = str ++ replicate (size - n `max` 0) ' '
  where n = length str
