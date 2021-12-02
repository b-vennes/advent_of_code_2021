module Main where

import Lib

main :: IO ()
main = do
  followSimpleCoursePrintHash "Input.txt"
  followAimedCoursePrintHash "Input.txt"
