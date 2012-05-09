{-# LANGUAGE TemplateHaskell #-}
module Main where

import Test.HUnit

import Test.Framework.Providers.HUnit
import Test.Framework.TH.Prime

import System.Exit
import System.Process

exampleTestCase :: String -> Assertion
exampleTestCase name = do
  let executable = "dist/build/" ++ name ++ "/" ++ name
  (code, out, err) <- readProcessWithExitCode executable [] ""
  expected <- readFile $ "tests/" ++ name ++ ".err.txt"

  code @=? ExitFailure 1
  out @=? ""
  err @=? expected

case_hello :: Assertion
case_hello = exampleTestCase "hello"

case_modes :: Assertion
case_modes = exampleTestCase "modes"

main :: IO ()
main = $(defaultMainGenerator)
