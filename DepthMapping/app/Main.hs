module Main where

import Lib

main :: IO ()
main = do
  countIncreasesOfSingleWindowAndPrint "Input.txt"
  countIncreasesOfTripleWindowAndPrint "Input.txt"
