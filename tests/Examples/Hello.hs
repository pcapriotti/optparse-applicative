{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
module Examples.Hello where

import Options.Applicative
import Data.Semigroup ((<>))
import Control.Monad (replicateM_)

import System.OsString (OsString, osstr)
import qualified "os-string" System.OsString as OsString
import Options.Applicative.Help (osStringToStrictText)
import qualified Data.Text.IO as Strict.IO

data Sample = Sample
  { hello  :: OsString
  , quiet  :: Bool
  , repeat :: Int }
  deriving Show

sample :: Parser Sample
sample = Sample
      <$> osStrOption
          ( long [osstr|hello|]
         <> metavar "TARGET"
         <> help "Target for the greeting" )
      <*> switch
          ( long [osstr|quiet|]
         <> short (OsString.unsafeFromChar 'q')
         <> help "Whether to be quiet" )
      <*> option auto
          ( long [osstr|repeat|]
         <> help "Repeats for greeting"
         <> showDefault
         <> value 1
         <> metavar "INT" )

main :: IO ()
main = greet =<< execParser opts

opts :: ParserInfo Sample
opts = info (sample <**> helper)
  ( fullDesc
  <> progDesc "Print a greeting for TARGET"
  <> header "hello - a test for optparse-applicative" )

greet :: Sample -> IO ()
greet (Sample h False n) = replicateM_ n . Strict.IO.putStrLn $ "Hello, " <> (osStringToStrictText h)
greet _ = return ()
