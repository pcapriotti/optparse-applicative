{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Examples.LongSub where

import Data.Monoid
import Options.Applicative

import System.OsString (OsString, osstr)

data Sample
  = Hello [OsString]
  | Goodbye
  deriving (Eq, Show)

hello :: Parser Sample
hello =
  Hello
    <$> many (argument osStr (metavar "TARGET..."))
    <*  switch (long [osstr|first-flag|])
    <*  switch (long [osstr|second-flag|])
    <*  switch (long [osstr|third-flag|])
    <*  switch (long [osstr|fourth-flag|])

sample :: Parser Sample
sample = hsubparser
       ( command [osstr|hello-very-long-sub|]
         (info hello
               (progDesc "Print greeting"))
       )

opts :: ParserInfo Sample
opts = info (sample <**> helper) idm
