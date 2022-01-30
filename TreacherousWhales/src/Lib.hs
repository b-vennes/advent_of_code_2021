module Lib
  ( Crab(Crab)
  , Crabs(Crabs)
  , pos
  , range
  , fuelRequired
  , exponentialFuelBurn
  , totalFuelForPosition
  , whaleAvoidance
  ) where

import Data.List (sort, sum)
import GHC.Base (Ord)

newtype Crab = Crab Int deriving (Show, Eq, Ord)

newtype Crabs = Crabs [Crab] deriving (Show, Eq)

pos :: Crab -> Int 
pos (Crab p) = p

range :: Crabs -> [Int]
range (Crabs crabs) =
  let sortedCrabs = sort crabs
      left = head sortedCrabs
      right = last sortedCrabs
  in [pos left..pos right]

fuelRequired :: Crab -> Int -> Int
fuelRequired (Crab cPos) position = abs (position - cPos)

exponentialFuelBurn :: Crab -> Int -> Int
exponentialFuelBurn (Crab cPos) target = sum [0..abs (target - cPos)]

totalFuelForPosition :: Crabs -> Int -> Int
totalFuelForPosition (Crabs crabs) position = sum $ map (`exponentialFuelBurn` position) crabs

whaleAvoidance :: Crabs -> Int
whaleAvoidance crabs =
  let crabsRange = range crabs
      fuelRange = map (totalFuelForPosition crabs) crabsRange
      sortedRange = sort fuelRange
  in head sortedRange
