module Options.Applicative.Utils where

(<+>) :: String -> String -> String
"" <+> s = s
s <+> "" = s
s1 <+> s2 = s1 ++ " " ++ s2
