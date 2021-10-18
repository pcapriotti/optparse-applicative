{-# LANGUAGE CPP #-}
module Examples.LongSub where

import Data.Monoid
import Options.Applicative

#if __GLASGOW_HASKELL__ <= 702
(<>) :: Monoid a => a -> a -> a
(<>) = mappend
#endif

data Sample
  = Hello [String]
  | Goodbye
  deriving (Eq, Show)

hello :: Parser Sample
hello =
  Hello
    <$> many (argument str (metavar "TARGET..."))
    <*  switch (long "first-flag")
    <*  switch (long "second-flag")
    <*  switch (long "third-flag")
    <*  switch (long "fourth-flag")

sample :: Parser Sample
sample = hsubparser
       ( command "hello-very-long-sub"
         (info hello
               (progDesc "Print greeting"))
       )

opts :: ParserInfo Sample
opts = info (sample <**> helper) idm
