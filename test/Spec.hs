module Main (main) where

import Test.Tasty

import Test.ActionTest

main :: IO ()
main = defaultMain runTests
