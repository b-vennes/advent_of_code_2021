module Lib where

import FileIO

data Movement = Forward Int | Up Int | Down Int deriving (Eq, Show)

parseWords :: [String] -> Movement
parseWords (direction : value : _)
  | direction == "forward" = Forward (read value :: Int)
  | direction == "up" = Up (read value :: Int)
  | direction == "down" = Down (read value :: Int)
  | otherwise = Forward 0
parseWords _ = Forward 0

instance TextParsable Movement where
  fromText text = parseWords $ words text

class MovementController m where
  up :: Int -> m -> m
  down :: Int -> m -> m
  forward :: Int -> m -> m

data SubPositionSimple = SubPositionSimple { longitude :: Int, depth :: Int } deriving (Eq, Show)

data SubPositionAimed = SubPositionAimed { aLongitude :: Int, aDepth :: Int, aim :: Int } deriving (Eq, Show)

instance MovementController SubPositionSimple where
  forward x (SubPositionSimple l d) = SubPositionSimple (l + x) d
  up x (SubPositionSimple l d) = SubPositionSimple l $ d - x
  down x (SubPositionSimple l d) = SubPositionSimple l $ d + x

instance MovementController SubPositionAimed where
    forward x (SubPositionAimed l d a) = SubPositionAimed (l + x) (d + x * a) a
    up x (SubPositionAimed l d a) = SubPositionAimed l d $ a - x
    down x (SubPositionAimed l d a) = SubPositionAimed l d $ a + x

class Hashable h where
  hash :: h -> Int

instance Hashable SubPositionSimple where
  hash (SubPositionSimple l d) = l * d

instance Hashable SubPositionAimed where
  hash (SubPositionAimed l d _) = l * d

followCourse :: MovementController m => [Movement] -> m -> m
followCourse (Forward x : t) sub = followCourse t $ forward x sub
followCourse (Up x : t) sub = followCourse t $ up x sub
followCourse (Down x : t) sub = followCourse t $ down x sub
followCourse [] sub = sub

parseCourse :: FilePath -> IO [Movement]
parseCourse path = parseLines path

followSimpleCoursePrintHash :: FilePath -> IO ()
followSimpleCoursePrintHash inputPath = do
  course <- parseCourse inputPath
  print $ "simple: " ++ (show $ hash $ followCourse course (SubPositionSimple 0 0))

followAimedCoursePrintHash :: FilePath -> IO ()
followAimedCoursePrintHash inputPath = do
  course <- parseCourse inputPath
  print $ "aimed: " ++ (show $ hash $ followCourse course (SubPositionAimed 0 0 0))
