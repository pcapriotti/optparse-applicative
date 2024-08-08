{-# LANGUAGE NamedFieldPuns #-}

module Examples.ParserGroup.Duplicates (opts, main) where

import Data.Semigroup ((<>))
import Options.Applicative

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
  { logPath :: Maybe String,
    logVerbosity :: Maybe Int
  }
  deriving (Show)

data LogGroup2 = LogGroup2
  { logNamespace :: String
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
  { hello :: String,
    logGroup1 :: LogGroup1,
    quiet :: Bool,
    systemGroup1 :: SystemGroup1,
    systemGroup2 :: SystemGroup2,
    logGroup2 :: LogGroup2,
    verbosity :: Int,
    cmd :: String
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
      strOption
        ( long "hello"
            <> metavar "TARGET"
            <> help "Target for the greeting"
        )

    parseLogGroup1 =
      parserOptionGroup "Logging" $
        LogGroup1
          <$> optional
            ( strOption
                ( long "file-log-path"
                    <> metavar "PATH"
                    <> help "Log file path"
                )
            )
          <*> optional
            ( option
                auto
                ( long "file-log-verbosity"
                    <> metavar "INT"
                    <> help "File log verbosity"
                )
            )

    parseQuiet =
      switch
        ( long "quiet"
            <> short 'q'
            <> help "Whether to be quiet"
        )

    parseSystemGroup1 =
      parserOptionGroup "System" $
        SystemGroup1
          <$> switch
            ( long "poll"
                <> help "Whether to poll"
            )
          <*> option
                auto
                ( long "timeout"
                    <> metavar "INT"
                    <> help "Whether to time out"
                )

    parseSystemGroup2 =
      parserOptionGroup "System" $
        SystemGroup2
          <$> switch
            ( long "sysFlag"
                <> help "Some flag"
            )

    parseLogGroup2 =
      parserOptionGroup "Logging" $
        LogGroup2
            <$>
              strOption
                ( long "log-namespace"
                    <> metavar "STR"
                    <> help "Log namespace"
                )

    parseVerbosity =
      option
        auto
        ( long "verbosity"
            <> short 'v'
            <> help "Console verbosity"
        )

    parseCmd = argument str (metavar "Command")

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

