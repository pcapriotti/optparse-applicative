{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PackageImports #-}

module Examples.ParserGroup.DuplicateCommandGroups (opts, main) where

import Data.Semigroup ((<>))
import Options.Applicative

import System.OsString (OsString, osstr)
import qualified "os-string" System.OsString as OsString

-- This test demonstrates that duplicate + consecutive groups are merged,
-- while duplicate + non-consecutive groups are not merged.

data Command
  = Delete
  | Insert
  | List
  | Print
  | Query
  deriving (Show)

data Sample = Sample
  { hello :: OsString,
    quiet :: Bool,
    verbosity :: Int,
    cmd :: Command
  }
  deriving (Show)

sample :: Parser Sample
sample =
  Sample
    <$> parseHello
    <*> parseQuiet
    <*> parseVerbosity
    <*> parseCommand

  where
    parseHello =
      osStrOption
        ( long [osstr|hello|]
            <> metavar "TARGET"
            <> help "Target for the greeting"
        )

    parseQuiet =
      switch
        ( long [osstr|quiet|]
            <> short (OsString.unsafeFromChar 'q')
            <> help "Whether to be quiet"
        )

    parseVerbosity =
      option
        auto
        ( long [osstr|verbosity|]
            <> short (OsString.unsafeFromChar 'v')
            <> help "Console verbosity"
        )

    parseCommand =
      hsubparser
        ( command [osstr|list|] (info (pure List) $ progDesc "Lists elements")
            <> commandGroup [osstr|Info commands|]
        )
        <|> hsubparser
          ( command [osstr|delete|] (info (pure Delete) $ progDesc "Deletes elements")
              <> commandGroup [osstr|Update commands|]
          )
        <|> hsubparser
          ( command [osstr|insert|] (info (pure Insert) $ progDesc "Inserts elements")
              <> commandGroup [osstr|Update commands|]
          )
        <|> hsubparser
          ( command [osstr|query|] (info (pure Query) $ progDesc "Runs a query")
          )
        <|> hsubparser
        ( command [osstr|print|] (info (pure Print) $ progDesc "Prints table")
            <> commandGroup [osstr|Info commands|]
        )

opts :: ParserInfo Sample
opts =
  info
    (sample <**> helper)
    ( fullDesc
        <> progDesc "Duplicate consecutive command groups consolidated"
        <> header "parser_group.duplicate_command_groups - a test for optparse-applicative"
    )

main :: IO ()
main = do
  r <- customExecParser (prefs helpShowGlobals) opts
  print r
