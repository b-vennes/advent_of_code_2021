module Main where

import Lib
import Parsing
import Text.Parsec

main :: IO ()
main = do
  input <- readFile "Input.txt"
  let ventLinesResult = parse ventLinesParser "" input
  let updatedGrid = fmap (\ls -> applyVentLines ls empty) ventLinesResult
  (either (\f -> print f) (\g -> print $ dangerCount g) updatedGrid)
