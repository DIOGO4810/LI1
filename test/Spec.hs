module Main where

import Test.HUnit

test_suite_01 = test ["Basic Test" ~: True ~=? True]

main :: IO ()
main = runTestTTAndExit $ test [test_suite_01]
