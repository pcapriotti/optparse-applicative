{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}

module Examples.ParserGroup.Basic (opts, main) where

import Data.Semigroup ((<>))
import Options.Applicative

import System.OsString (OsString, osstr)
import qualified "os-string" System.OsString as OsString

data LogGroup = LogGroup
  { logPath :: Maybe OsString,
    logVerbosity :: Maybe Int
  }
  deriving (Show)

data SystemGroup = SystemGroup
  { poll :: Bool,
    timeout :: Int
  }
  deriving (Show)

data Sample = Sample
  { hello :: OsString,
    logGroup :: LogGroup,
    quiet :: Bool,
    systemGroup :: SystemGroup,
    verbosity :: Int,
    cmd :: OsString
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
      osStrOption
        ( long [osstr|hello|]
            <> metavar "TARGET"
            <> help "Target for the greeting"
        )

    parseLogGroup =
      parserOptionGroup [osstr|Logging|] $
        LogGroup
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

    parseSystemGroup =
      parserOptionGroup [osstr|System Options|] $
        SystemGroup
          <$> switch
            ( long [osstr|poll|]
                <> help "Whether to poll"
            )
          <*> ( option
                  auto
                  ( long [osstr|timeout|]
                      <> metavar "INT"
                      <> help "Whether to time out"
                  )
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
        <> progDesc "Shows parser groups"
        <> header "parser_group.basic - a test for optparse-applicative"
    )

main :: IO ()
main = do
  r <- customExecParser (prefs helpShowGlobals) opts
  print r
