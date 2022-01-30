module Lib where

import Data.Map (Map, lookup, insert, empty, filter, size)

data Coordinate = Coordinate { x :: Int, y :: Int } deriving (Eq, Show, Ord)

fromPair :: (Int, Int) -> Coordinate
fromPair (a, b) = Coordinate a b

data VentLine = VentLine { pointA :: Coordinate, pointB :: Coordinate } deriving (Eq, Show)

createVentLine :: Int -> Int -> Int -> Int -> VentLine
createVentLine x1 x2 x3 x4 = VentLine (Coordinate x1 x2) (Coordinate x3 x4)

coordinates :: VentLine -> [Coordinate]
coordinates (VentLine (Coordinate x1 y1) (Coordinate x2 y2))
  | x1 == x2 && y1 <= y2 = map (Coordinate x1) [y1..y2]
  | x1 == x2 && y2 <= y1 = map (Coordinate x1) [y2..y1]
  | y1 == y2 && x1 <= x2 = map (`Coordinate` y1) [x1..x2]
  | y1 == y2 && x2 <= x1 = map (`Coordinate` y1) [x2..x1]
  | x1 <= x2 && y1 <= y2 = zipWith (curry fromPair) [x1..x2] [y1..y2]
  | x1 <= x2 && y2 <= y1 = zipWith (curry fromPair) [x1..x2] (reverse [y2..y1])
  | x2 <= x1 && y1 <= y2 = zipWith (curry fromPair) (reverse [x2..x1]) [y1..y2]
  | otherwise = zipWith (curry fromPair) (reverse [x2..x1]) (reverse [y2..y1])

type Grid = Map Coordinate Int

empty :: Grid
empty = Data.Map.empty

increment :: Coordinate -> Grid -> Grid
increment coordinate grid =
  getOrAddValue $ do
                    current <- Data.Map.lookup coordinate grid
                    return (Data.Map.insert coordinate (current + 1) grid) where
      getOrAddValue :: Maybe Grid -> Grid
      getOrAddValue Nothing = Data.Map.insert coordinate 1 grid
      getOrAddValue (Just next) = next

applyVentLine :: VentLine -> Grid -> Grid
applyVentLine ventLine grid =
  foldl (flip increment) grid (coordinates ventLine)

applyVentLines :: [VentLine] -> Grid -> Grid
applyVentLines remaining grid
  = foldl (flip applyVentLine) grid remaining

dangerCount :: Grid -> Int
dangerCount grid = Data.Map.size $ Data.Map.filter (> 1) grid
