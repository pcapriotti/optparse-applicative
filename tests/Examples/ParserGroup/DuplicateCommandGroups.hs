{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Examples.ParserGroup.DuplicateCommandGroups (opts, main) where

import Data.Semigroup ((<>))
import Options.Applicative

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
  { hello :: String,
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
      strOption
        ( long "hello"
            <> metavar "TARGET"
            <> help "Target for the greeting"
        )

    parseQuiet =
      switch
        ( long "quiet"
            <> short 'q'
            <> help "Whether to be quiet"
        )

    parseVerbosity =
      option
        auto
        ( long "verbosity"
            <> short 'v'
            <> help "Console verbosity"
        )

    parseCommand =
      hsubparser
        ( command "list" (info (pure List) $ progDesc "Lists elements")
            <> commandGroup "Info commands"
        )
        <|> hsubparser
          ( command "delete" (info (pure Delete) $ progDesc "Deletes elements")
              <> commandGroup "Update commands"
          )
        <|> hsubparser
          ( command "insert" (info (pure Insert) $ progDesc "Inserts elements")
              <> commandGroup "Update commands"
          )
        <|> hsubparser
          ( command "query" (info (pure Query) $ progDesc "Runs a query")
          )
        <|> hsubparser
        ( command "print" (info (pure Print) $ progDesc "Prints table")
            <> commandGroup "Info commands"
        )

opts :: ParserInfo Sample
opts =
  info
    (sample <**> helper)
    ( progDesc "Duplicate consecutive command groups consolidated"
        <> header "parser_group.duplicate_command_groups - a test for optparse-applicative"
    )

main :: IO ()
main = do
  r <- customExecParser (prefs helpShowGlobals) opts
  print r
