{-# LANGUAGE PackageImports #-}
module Examples.Alternatives where

import Options.Applicative

import qualified "os-string" System.OsString as OsString

data Value = A | B
  deriving (Eq, Show)

values :: Parser [Value]
values = many $ a <|> b

a :: Parser Value
a = flag' A (short (OsString.unsafeFromChar 'a'))

b :: Parser Value
b = flag' B (short (OsString.unsafeFromChar 'b'))

opts :: ParserInfo [Value]
opts = info values idm
