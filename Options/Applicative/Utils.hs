module Options.Applicative.Utils where

(<+>) :: String -> String -> String
"" <+> s = s
s <+> "" = s
s1 <+> s2 = s1 ++ " " ++ s2

tabulate :: Int -> [(String, String)] -> String
tabulate size table = unlines
  [ "  " ++ pad size key ++ " " ++ value
  | (key, value) <- table ]

tabulate' :: [(String, String)] -> String
tabulate' = tabulate 24

pad :: Int -> String -> String
pad size str = str ++ replicate (size - n `max` 0) ' '
  where n = length str
