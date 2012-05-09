module Options.Applicative.Help where

import Options.Applicative
import Options.Applicative.Types

parserHelpText :: ParserInfo a -> String
parserHelpText pinfo = unlines
   $ nn [infoHeader pinfo]
  ++ [ "  " ++ line | line <- nn [infoProgDesc pinfo] ]
  ++ [ line | desc <- nn [fullDesc p]
            , line <- ["", "Common options:", desc]
            , infoFullDesc pinfo ]
  ++ [ line | footer <- nn [infoFooter pinfo]
            , line <- ["", footer] ]
  where
    nn = filter (not . null)
    p = infoParser pinfo
