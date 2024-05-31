{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Examples.ParserGroup.CommandGroups (opts) where

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

data Command
  = Delete
  | List
  | Print
  | Query
  deriving (Show)

data Sample = Sample
  { hello :: String,
    logGroup :: LogGroup,
    quiet :: Bool,
    systemGroup :: SystemGroup,
    verbosity :: Int,
    cmd :: Command
  }
  deriving (Show)

sample :: Parser Sample
sample = do
  hello <- parseHello
  logGroup <- parseLogGroup
  quiet <- parseQuiet
  systemGroup <- parseSystemGroup
  verbosity <- parseVerbosity
  cmd <- parseCommand

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

    parseCommand =
      hsubparser
        ( command "list 2" (info (pure List) $ progDesc "Lists elements")
        )
        <|> hsubparser
        ( command "list" (info (pure List) $ progDesc "Lists elements")
            <> command "print" (info (pure Print) $ progDesc "Prints table")
            <> commandGroup "Info commands"
        )
        <|> hsubparser
          ( command "delete" (info (pure Delete) $ progDesc "Deletes elements")
          )
        <|> hsubparser
          ( command "query" (info (pure Query) $ progDesc "Runs a query")
              <> commandGroup "Query commands"
          )

opts :: ParserInfo Sample
opts =
  info
    (sample <**> helper)
    ( fullDesc
        <> progDesc "Option and command groups"
        <> header "parser_group.command_groups - a test for optparse-applicative"
    )
