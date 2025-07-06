{-# LANGUAGE NamedFieldPuns #-}

module Examples.ParserGroup.Basic (opts, main) where

import Data.Semigroup ((<>))
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
sample =
  Sample
    <$> parseHello
    <*> parseLogGroup
    <*> parseQuiet
    <*> parseSystemGroup
    <*> parseVerbosity
    <*> parseCmd

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

    parseCmd = argument str (metavar "Command")

opts :: ParserInfo Sample
opts =
  info
    (sample <**> helper)
    (  progDesc "Shows parser groups"
        <> header "parser_group.basic - a test for optparse-applicative"
    )

main :: IO ()
main = do
  r <- customExecParser (prefs helpShowGlobals) opts
  print r
