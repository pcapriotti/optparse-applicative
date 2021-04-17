{-# LANGUAGE CPP #-}
module Examples.Hello where

import Options.Applicative
import Control.Monad (replicateM_, guard)
import Options.Applicative.Types
import Options.Applicative.Common
import System.IO

data Sample = Sample
  { hello  :: String
  , quiet  :: Bool
  , repeat :: Int }
  deriving Show

deprecated :: [OptName] -> ReadM OptName a -> ReadM OptName (Maybe String, a)
deprecated names rdr = do
  name <- readerName
  x <- rdr
  let depr = showOption name <$ guard (name `elem` names)
  pure (depr, x)

sample :: Parser (Maybe String, Sample)
sample = mk
      <$> option (deprecated [OptLong "helo"] str)
          ( long "hello"
         <> long "helo"
         <> metavar "TARGET"
         <> help "Target for the greeting" )
      <*> switch
          ( long "quiet"
         <> short 'q'
         <> help "Whether to be quiet" )
      <*> option auto
          ( long "repeat"
         <> help "Repeats for greeting"
         <> showDefault
         <> value 1
         <> metavar "INT" )
  where
    mk (d, t) q n = (d, Sample t q n)

main :: IO ()
main = greet =<< execParser opts
  where
    opts = info (sample <**> helper)
      ( fullDesc
      <> progDesc "Print a greeting for TARGET"
      <> header "hello - a test for optparse-applicative" )

greet :: (Maybe String, Sample) -> IO ()
greet (depr, Sample h False n) = do
  case depr of
    Just name -> hPutStrLn stderr $ "option " ++ name ++ " is deprecated"
    _ -> pure ()
  replicateM_ n . putStrLn $ "Hello, " ++ h
greet _ = return ()
