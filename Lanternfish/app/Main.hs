module Main where

import Lib ( lanternfishProgram )
import Parsing ( parsingLanternfishProgram )

main :: IO ()
main = do
  input <- readFile "Input.txt"
  let parsedInput = parsingLanternfishProgram input
  let result = fmap (`lanternfishProgram` 256) parsedInput
  print result
