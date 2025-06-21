{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}

module Examples.ParserGroup.Duplicates (opts, main) where

import Data.Semigroup ((<>))
import Options.Applicative

import System.OsString (OsString, osstr)
import qualified "os-string" System.OsString as OsString

-- NOTE: This is the same structure as ParserGroup.Basic __except__
-- we have two (non-consecutive) "Logging" groups and two (consecutive)
-- System groups. This test demonstrates two things:
--
-- 1. Non-consecutive groups are not merged (i.e. we display two "Logging"
--    sections).
-- 2. Consecutive groups are merged (i.e. we display only one "System" group).
--
-- This is like command groups.

data LogGroup1 = LogGroup1
  { logPath :: Maybe OsString,
    logVerbosity :: Maybe Int
  }
  deriving (Show)

data LogGroup2 = LogGroup2
  { logNamespace :: OsString
  }
  deriving (Show)

data SystemGroup1 = SystemGroup1
  { poll :: Bool,
    timeout :: Int
  }
  deriving (Show)

newtype SystemGroup2 = SystemGroup2
  { sysFlag :: Bool
  }
  deriving (Show)

data Sample = Sample
  { hello :: OsString,
    logGroup1 :: LogGroup1,
    quiet :: Bool,
    systemGroup1 :: SystemGroup1,
    systemGroup2 :: SystemGroup2,
    logGroup2 :: LogGroup2,
    verbosity :: Int,
    cmd :: OsString
  }
  deriving (Show)

sample :: Parser Sample
sample =
  Sample
    <$> parseHello
    <*> parseLogGroup1
    <*> parseQuiet
    <*> parseSystemGroup1
    <*> parseSystemGroup2
    <*> parseLogGroup2
    <*> parseVerbosity
    <*> parseCmd

  where
    parseHello =
      osStrOption
        ( long [osstr|hello|]
            <> metavar "TARGET"
            <> help "Target for the greeting"
        )

    parseLogGroup1 =
      parserOptionGroup [osstr|Logging|] $
        LogGroup1
          <$> optional
            ( osStrOption
                ( long [osstr|file-log-path|]
                    <> metavar "PATH"
                    <> help "Log file path"
                )
            )
          <*> optional
            ( option
                auto
                ( long [osstr|file-log-verbosity|]
                    <> metavar "INT"
                    <> help "File log verbosity"
                )
            )

    parseQuiet =
      switch
        ( long [osstr|quiet|]
            <> short (OsString.unsafeFromChar 'q')
            <> help "Whether to be quiet"
        )

    parseSystemGroup1 =
      parserOptionGroup [osstr|System|] $
        SystemGroup1
          <$> switch
            ( long [osstr|poll|]
                <> help "Whether to poll"
            )
          <*> option
                auto
                ( long [osstr|timeout|]
                    <> metavar "INT"
                    <> help "Whether to time out"
                )

    parseSystemGroup2 =
      parserOptionGroup [osstr|System|] $
        SystemGroup2
          <$> switch
            ( long [osstr|sysFlag|]
                <> help "Some flag"
            )

    parseLogGroup2 =
      parserOptionGroup [osstr|Logging|] $
        LogGroup2
            <$>
              osStrOption
                ( long [osstr|log-namespace|]
                    <> metavar "STR"
                    <> help "Log namespace"
                )

    parseVerbosity =
      option
        auto
        ( long [osstr|verbosity|]
            <> short (OsString.unsafeFromChar 'v')
            <> help "Console verbosity"
        )

    parseCmd = argument osStr (metavar "Command")

opts :: ParserInfo Sample
opts =
  info
    (sample <**> helper)
    ( fullDesc
        <> progDesc "Duplicate consecutive groups consolidated"
        <> header "parser_group.duplicates - a test for optparse-applicative"
    )

main :: IO ()
main = do
  r <- customExecParser (prefs helpShowGlobals) opts
  print r

