{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
module Examples.Commands where

import Data.List
import Data.Monoid
import Options.Applicative

import System.OsString (OsString, osstr)
import qualified "os-string" System.OsString as OsString
import qualified Data.Text as Strict
import qualified Data.Text.IO as Strict.IO
import Options.Applicative.Help (osStringToStrictText)

data Sample
  = Hello [OsString]
  | Goodbye
  deriving (Eq, Show)

hello :: Parser Sample
hello = Hello <$> many (argument osStr (metavar "TARGET..."))

sample :: Parser Sample
sample = subparser
       ( command [osstr|hello|]
         (info hello
               (progDesc "Print greeting"))
      <> command [osstr|goodbye|]
         (info (pure Goodbye)
               (progDesc "Say goodbye"))
       )
      <|> subparser
       ( command [osstr|bonjour|]
         (info hello
               (progDesc "Print greeting"))
      <> command [osstr|au-revoir|]
         (info (pure Goodbye)
               (progDesc "Say goodbye"))
      <> commandGroup [osstr|French commands:|]
      <> hidden
       )

run :: Sample -> IO ()
run (Hello targets) = Strict.IO.putStrLn $ "Hello, " <> Strict.intercalate ", " (osStringToStrictText <$> targets) <> "!"
run Goodbye = Strict.IO.putStrLn "Goodbye."

opts :: ParserInfo Sample
opts = info (sample <**> helper) idm

main :: IO ()
main = execParser opts >>= run
