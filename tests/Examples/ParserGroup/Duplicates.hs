{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NamedFieldPuns #-}

module Examples.ParserGroup.Duplicates (opts) where

import Options.Applicative

data LogGroup = LogGroup
  { logPath :: Maybe String,
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
    systemGroup :: SystemGroup,
    verbosity :: Int,
    cmd :: String
  }
  deriving (Show)

sample :: Parser Sample
sample = do
  hello <- parseHello
  logGroup <- parseLogGroup
  quiet <- parseQuiet
  systemGroup <- parseSystemGroup
  verbosity <- parseVerbosity
  cmd <- parseCmd

  pure $
    Sample
      { hello,
        logGroup,
        quiet,
        systemGroup,
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

    parseLogGroup =
      parserOptionGroup "Logging" $
        LogGroup
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

    -- NOTE: This is the same structure as ParserGroup.Basic __except__
    -- parserSystemGroup has the same group ("Logging") as parseLogGroup. This
    -- tests shows that identical group names are consolidated.
    parseSystemGroup =
      parserOptionGroup "Logging" $
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
        <> progDesc "Duplicate groups consolidated"
        <> header "parser_group.duplicates - a test for optparse-applicative"
    )
