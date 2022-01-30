module Lib (
    tick
  , age
  , filterByAge
  , simulateDays
  , lanternfishProgram
  , Lanternfish(Lanternfish)
  , createPool
  , createPoolWithCounts
  , LanternfishPool(LanternfishPool)
  , mapIndex
  ) where

import Data.List (drop, take, zip)

newtype Lanternfish = Lanternfish Int deriving (Eq, Show)

age :: Lanternfish -> Int
age (Lanternfish a) = a

filterByAge :: [Lanternfish] -> Int -> [Lanternfish]
filterByAge fish days = filter ((== days) . age) fish

newtype LanternfishPool = LanternfishPool [Int] deriving (Eq, Show)

createPoolWithCounts :: [Int] -> LanternfishPool
createPoolWithCounts counts = LanternfishPool $ take 9 counts

createPool :: [Lanternfish] -> LanternfishPool
createPool fishies =
  let validAges = [0..8]
      ageCounts = map (length . filterByAge fishies) validAges
  in LanternfishPool ageCounts

totalCount :: LanternfishPool -> Int
totalCount (LanternfishPool counts) = sum counts

mapIndex :: Int -> (a -> a) -> [a] -> [a]
mapIndex _ _ [] = []
mapIndex index f (x:xs)
  | index == 0 = f x : xs
  | otherwise  = x : mapIndex (index - 1) f xs

tick :: LanternfishPool -> LanternfishPool
tick (LanternfishPool counts) =
  let completedCount = head counts
      rotated = tail counts ++ [completedCount]
  in createPoolWithCounts $ mapIndex 6 (+ completedCount) rotated

simulateDays :: LanternfishPool -> Int -> LanternfishPool
simulateDays pool days
  | days <= 0 = pool
  | otherwise = simulateDays (tick pool) (days - 1)

lanternfishProgram :: [Lanternfish] -> Int -> Int
lanternfishProgram inputFish days =
  let startingPool = createPool inputFish
      finalPool = startingPool `simulateDays` days
  in totalCount finalPool
