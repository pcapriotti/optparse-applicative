module Examples.Alternatives where

import Options.Applicative

data Value = A | B
  deriving (Eq, Show)

values :: Parser ann [Value]
values = many $ a <|> b

a :: Parser ann Value
a = flag' A (short 'a')

b :: Parser ann Value
b = flag' B (short 'b')

opts :: ParserInfo ann [Value]
opts = info values idm
