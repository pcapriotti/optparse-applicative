{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NamedFieldPuns #-}

module Examples.ParserGroup.Nested (opts) where

import Options.Applicative

-- Nested groups. Demonstrates that the "outer group" overrides any "inner"
-- groups i.e. groups cannot be nested.

data LogGroup = LogGroup
  { logPath :: Maybe String,
    systemGroup :: SystemGroup,
    logVerbosity :: Maybe Int
  }
  deriving (Show)

data SystemGroup = SystemGroup
  { poll :: Bool,
    timeout :: Int
  }
  deriving (Show)

data Sample = Sample
  { hello :: String,
    logGroup :: LogGroup,
    quiet :: Bool,
    verbosity :: Int,
    cmd :: String
  }
  deriving (Show)

sample :: Parser Sample
sample = do
  hello <- parseHello
  logGroup <- parseLogGroup
  quiet <- parseQuiet
  verbosity <- parseVerbosity
  cmd <- parseCmd

  pure $
    Sample
      { hello,
        logGroup,
        quiet,
        verbosity,
        cmd
      }
  where
    parseHello =
      strOption
        ( long "hello"
            <> metavar "TARGET"
            <> help "Target for the greeting"
        )

    parseLogGroup = parserOptionGroup "Logging" $ do
      logPath <- parseLogPath
      systemGroup <- parseSystemGroup
      logVerbosity <- parseLogVerbosity
      pure $
        LogGroup
          { logPath,
            systemGroup,
            logVerbosity
          }
      where
        parseLogPath =
          optional
            ( strOption
                ( long "file-log-path"
                    <> metavar "PATH"
                    <> help "Log file path"
                )
            )
        parseLogVerbosity =
          optional
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

    parseSystemGroup =
      parserOptionGroup "System Options" $
        SystemGroup
          <$> switch
            ( long "poll"
                <> help "Whether to poll"
            )
          <*> ( option
                  auto
                  ( long "timeout"
                      <> metavar "INT"
                      <> help "Whether to time out"
                  )
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
        <> progDesc "Nested parser groups"
        <> header "parser_group.nested - a test for optparse-applicative"
    )
