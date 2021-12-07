module Lib where

import FileRead
import Control.Arrow

newtype BingoCard = BingoCard [(Bool, Int)] deriving (Eq, Show)

newtype BingoCards = BingoCards [BingoCard] deriving (Eq, Show)

newtype Numbers = Numbers [Int] deriving (Eq, Show)

data BingoCardFailure = InvalidLength deriving (Eq, Show)

createBingoCard :: [Int] -> Either BingoCardFailure BingoCard
createBingoCard values
  | length values == 25 = Right $ BingoCard (map (\x -> (False, x)) values)
  | otherwise = Left InvalidLength

mark :: Int -> BingoCards -> BingoCards
mark value (BingoCards cards) = BingoCards $ map markCard cards where
  markCard :: BingoCard -> BingoCard
  markCard (BingoCard values) = BingoCard (map (\(marked, x) -> ((marked || x == value), x)) values)

score :: BingoCard -> Int -> Int
score (BingoCard values) lastCalled = lastCalled * (sumUnmarked values) where
  sumUnmarked :: [(Bool, Int)] -> Int
  sumUnmarked [] = 0
  sumUnmarked ((False, value) : rest) = value + (sumUnmarked rest)
  sumUnmarked (_ : rest) = sumUnmarked rest

row :: Int -> BingoCard -> [(Bool, Int)]
row r (BingoCard values) = take 5 $ drop (5 * r) values

col :: Int -> BingoCard -> [(Bool, Int)]
col c (BingoCard values) = (zip [0 .. ((length values) - 1)] values) >>= (valueIfMod5 c) where
  valueIfMod5 :: Int -> ((Int, (Bool, Int)) -> [(Bool, Int)])
  valueIfMod5 x = \(idx, a) -> if (mod idx 5) == x then [a] else []

diagDown :: BingoCard -> [(Bool, Int)]
diagDown (BingoCard values) = [values !! 0, values !! 6, values !! 12, values !! 18, values !! 24]

diagUp :: BingoCard -> [(Bool, Int)]
diagUp (BingoCard values) = [values !! 4, values !! 8, values !! 12, values !! 16, values !! 20]

allMarked :: [(Bool, Int)] -> Bool
allMarked ((True, _) : t) = allMarked t
allMarked [] = True
allMarked _ = False

winner :: BingoCard -> Bool
winner card =
  any id $
    map (\x -> allMarked $ row x card) [0..4] ++
    map (\x -> allMarked $ col x card) [0..4] ++
    [allMarked $ diagDown card, allMarked $ diagUp card]

instance StringDeserializer Numbers where
  deserialize str = mapFailure (parseNumbers [] str) (\err -> "failed to parse bingo numbers: " ++ err) where
    parseNumbers :: [Int] -> String -> DeserializeResult Numbers
    parseNumbers values (next : remaining)
      | next == '\n' = Success $ (Numbers values, remaining)
      | otherwise = mapSuccess (deserialize (next : remaining)) (\(i, r) -> parseNumbers (values ++ [i]) r)
    parseNumbers values [] = Success $ (Numbers values, [])

instance StringDeserializer BingoCard where
  deserialize str = mapFailure (parseCard [] str) (\err -> "failed to parse bingo card: " ++ err) where
    parseCard :: [Int] -> String -> DeserializeResult BingoCard
    parseCard values remaining
      | length values == 25 = fromEither (left show (createBingoCard values)) remaining
      | otherwise = mapSuccess (deserialize remaining) (\(i, r) -> parseCard (values ++ [i]) r)

instance StringDeserializer BingoCards where
  deserialize str = parseCards [] str where
    parseCards :: [BingoCard] -> String -> DeserializeResult BingoCards
    parseCards cards [] = Success (BingoCards cards, [])
    parseCards cards (next : remaining)
      | next == '\n' || next == ' ' = parseCards cards remaining
      | otherwise = mapSuccess (deserialize (next : remaining)) (\(c, r) -> parseCards (cards ++ [c]) r)

findWinner :: Int -> BingoCards -> Maybe Int
findWinner latest (BingoCards cards) = getScore (filter winner cards) where
  getScore :: [BingoCard] -> Maybe Int
  getScore (w : _) = Just $ score w latest
  getScore [] = Nothing

findLoser :: Int -> BingoCards -> Maybe Int
findLoser latest (BingoCards cards) = getScoreIfLoser cards where
  getScoreIfLoser :: [BingoCard] -> Maybe Int
  getScoreIfLoser (loser : [])
    | winner loser = Just $ score loser latest
    | otherwise = Nothing
  getScoreIfLoser _ = Nothing

findWinningScore :: Numbers -> BingoCards -> Maybe Int
findWinningScore (Numbers (next : remaining)) cards =
  let markedWithNext = mark next cards in
    continueIfNoWinner (findWinner next markedWithNext) (markedWithNext) where
      continueIfNoWinner :: Maybe Int -> BingoCards -> Maybe Int
      continueIfNoWinner Nothing c = findWinningScore (Numbers remaining) c
      continueIfNoWinner result _ = result
findWinningScore (Numbers []) _ = Nothing

findLosingScore :: Numbers -> BingoCards -> Maybe Int
findLosingScore (Numbers (next : remaining)) cards =
  let markedWithNext = mark next cards in
    continueIfNoLoser (findLoser next markedWithNext) (markedWithNext) where
      continueIfNoLoser :: Maybe Int -> BingoCards -> Maybe Int
      continueIfNoLoser Nothing (BingoCards cs) =
        findLosingScore (Numbers remaining) $ BingoCards (filter (\card -> not $ winner card) cs)
      continueIfNoLoser result _ = result
findLosingScore (Numbers []) _ = Nothing

parseInput :: FilePath -> IO (DeserializeResult (Numbers, BingoCards))
parseInput path = do
  contents <- readFile path
  return $ deserialize $ contents

printParsedInput :: FilePath -> IO ()
printParsedInput input = do
  inputData <- parseInput input
  print inputData

printWinningScore :: FilePath -> IO ()
printWinningScore input = do
  inputData <- parseInput input
  print $ mapSuccess inputData (\((nums, boards), r) -> Success $ (findWinningScore nums boards, r))

printLosingScore :: FilePath -> IO ()
printLosingScore input = do
  inputData <- parseInput input
  print $ mapSuccess inputData (\((nums, boards), r) -> Success $ (findLosingScore nums boards, r))
