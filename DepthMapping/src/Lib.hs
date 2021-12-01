module Lib where

import FileIO

newtype Depth = Depth Int deriving (Eq, Show, Ord)

instance TextParsable Depth where
  fromText text = Depth (read text :: Int)

sumDepths :: [Depth] -> Int
sumDepths ((Depth a) : t) = a + (sumDepths t)
sumDepths [] = 0

countIncreasesSingleWindow :: [Depth] -> Int
countIncreasesSingleWindow (first : second : t)
  | first < second = 1 + (countIncreasesSingleWindow $ second : t)
  | otherwise = countIncreasesSingleWindow $ second : t
countIncreasesSingleWindow _ = 0

countIncreasesTripleWindow :: [Depth] -> Int
countIncreasesTripleWindow (first : second : third : fourth : t)
  | sumDepths [first, second, third] < sumDepths [second, third, fourth] =
    1 + (countIncreasesTripleWindow $ second : third : fourth : t)
  | otherwise = countIncreasesTripleWindow $ second : third : fourth : t
countIncreasesTripleWindow _ = 0

countIncreasesOfSingleWindowAndPrint :: String -> IO ()
countIncreasesOfSingleWindowAndPrint inputFile = do
  depths <- (parseLines inputFile :: IO [Depth])
  print $ "Single Window Depths of " ++ inputFile ++ ": " ++ show (countIncreasesSingleWindow depths)

countIncreasesOfTripleWindowAndPrint :: String -> IO ()
countIncreasesOfTripleWindowAndPrint inputFile = do
  depths <- (parseLines inputFile :: IO [Depth])
  print $ "Triple Window Depths of " ++ inputFile ++ ": " ++ show (countIncreasesTripleWindow depths)
