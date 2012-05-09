module Options.Applicative.Help where

import Options.Applicative
import Options.Applicative.Types

parserHelpText :: ParserInfo a -> String
parserHelpText info = unlines
   $ nn [infoHeader info]
  ++ [ "  " ++ line | line <- nn [infoProgDesc info] ]
  ++ [ line | desc <- nn [fullDesc p]
            , line <- ["", "Common options:", desc]
            , infoFullDesc info ]
  ++ [ line | footer <- nn [infoFooter info]
            , line <- ["", footer] ]
  where
    nn = filter (not . null)
    p = infoParser info
