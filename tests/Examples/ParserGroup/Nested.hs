{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}

module Examples.ParserGroup.Nested (opts, main) where

import Data.Semigroup ((<>))
import Options.Applicative

import System.OsString (OsString, osstr)
import qualified "os-string" System.OsString as OsString

-- Nested groups. Demonstrates that group can nest.

data LogGroup = LogGroup
  { logPath :: Maybe OsString,
    systemGroup :: SystemGroup,
    logVerbosity :: Maybe Int
  }
  deriving (Show)

data SystemGroup = SystemGroup
  { poll :: Bool,
    deepNested :: Nested2,
    timeout :: Int
  }
  deriving (Show)

data Nested2 = Nested2
  { nested2Str :: OsString,
    nested3 :: Nested3
  }
  deriving (Show)

newtype Nested3 = Nested3
  { nested3Str :: OsString
  }
  deriving (Show)

data Sample = Sample
  { hello :: OsString,
    logGroup :: LogGroup,
    quiet :: Bool,
    verbosity :: Int,
    group2 :: (Int, Int),
    cmd :: OsString
  }
  deriving (Show)

sample :: Parser Sample
sample =
  Sample
    <$> parseHello
    <*> parseLogGroup
    <*> parseQuiet
    <*> parseVerbosity
    <*> parseGroup2
    <*> parseCmd

  where
    parseHello =
      osStrOption
        ( long [osstr|hello|]
            <> metavar "TARGET"
            <> help "Target for the greeting"
        )

    parseLogGroup =
      parserOptionGroup [osstr|First group|] $
      parserOptionGroup [osstr|Second group|] $
      parserOptionGroup [osstr|Logging|] $
        LogGroup
          <$> parseLogPath
          <*> parseSystemGroup
          <*> parseLogVerbosity

      where
        parseLogPath =
          optional
            ( osStrOption
                ( long [osstr|file-log-path|]
                    <> metavar "PATH"
                    <> help "Log file path"
                )
            )
        parseLogVerbosity =
          optional
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
          <$> switch (long [osstr|poll|] <> help "Whether to poll")
          <*> parseNested2
          <*> option auto (long [osstr|timeout|] <> metavar "INT" <> help "Whether to time out")

    parseNested2 =
      parserOptionGroup [osstr|Nested2|] $
        Nested2
          <$> option osStr (long [osstr|double-nested|] <> metavar "STR" <> help "Some nested option")
          <*> parseNested3

    parseNested3 =
      parserOptionGroup [osstr|Nested3|] $
        Nested3 <$> option osStr (long [osstr|triple-nested|] <> metavar "STR" <> help "Another option")

    parseGroup2 :: Parser (Int, Int)
    parseGroup2 = parserOptionGroup [osstr|Group 2|] $
      (,)
        <$> parserOptionGroup [osstr|G 2.1|] (option auto (long [osstr|one|] <> help "Option 1"))
        <*> parserOptionGroup [osstr|G 2.2|] (option auto (long [osstr|two|] <> help "Option 2"))

    parseVerbosity =
      option auto (long [osstr|verbosity|] <> short (OsString.unsafeFromChar 'v') <> help "Console verbosity")

    parseCmd =
      argument osStr (metavar "Command")

opts :: ParserInfo Sample
opts =
  info
    (sample <**> helper)
    ( fullDesc
        <> progDesc "Nested parser groups"
        <> header "parser_group.nested - a test for optparse-applicative"
    )

main :: IO ()
main = do
  r <- customExecParser (prefs helpShowGlobals) opts
  print r
