{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
module Examples.Formatting where

import           Data.Monoid
import           Options.Applicative
import           Prelude

import System.OsString (osstr)
import qualified "os-string" System.OsString as OsString

opts :: Parser Int
opts = option auto $ mconcat
  [ long [osstr|test|]
  , short (OsString.unsafeFromChar 't')
  , value 0
  , metavar "FOO_BAR_BAZ_LONG_METAVARIABLE"
  , help "This is an options with a very very long description.  Hopefully, this will be nicely formatted by the help text generator." ]
