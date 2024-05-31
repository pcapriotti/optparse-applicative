{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NamedFieldPuns #-}

module Examples.ParserGroup.AllGrouped (opts) where

import Options.Applicative

-- Tests the help page when every option belongs to some group i.e. there are
-- no top-level options. Notice we put the helper (<**> helper) __inside__
-- one of the groups, so that it is not a top-level option.
--
-- Also notice that although we add cmdParser to the same group, it is __not__
-- rendered as part of this group. This is what we want, as it is an Argument
-- and should not be rendered with the Options.

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
  { logGroup :: LogGroup,
    systemGroup :: SystemGroup,
    cmd :: String
  }
  deriving (Show)

sample :: Parser Sample
sample = do
  logGroup <- parseLogGroup
  systemGroup <- parseSystemGroup
  cmd <- parseCmd

  pure $
    Sample
      { logGroup,
        systemGroup,
        cmd
      }
  where
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
            <**> helper

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

    parseCmd = argument str (metavar "Command")

opts :: ParserInfo Sample
opts =
  info
    sample
    ( fullDesc
        <> progDesc "Every option is grouped"
        <> header "parser_group.all_grouped - a test for optparse-applicative"
    )
