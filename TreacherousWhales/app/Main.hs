module Main where

import Lib (whaleAvoidance)
import Parsing (parseFile, parseCrabs)

main :: IO ()
main = do
	crabs <- parseFile "Input.txt" parseCrabs
	let result = fmap whaleAvoidance crabs
	print result