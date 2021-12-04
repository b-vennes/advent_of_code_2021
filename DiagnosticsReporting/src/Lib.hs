module Lib where

import FileIO

data Bit = Zero | One deriving (Show, Eq)

(***) :: Bit -> Int -> Int
(***) Zero _ = 0
(***) One x = x

instance TextParsable Bit where
  fromText text
    | text == "0" = Zero
    | text == "1" = One
    | otherwise = Zero

data DiagnosticEntry =
  DiagnosticEntry {
    b0 :: Bit
  , b1 :: Bit
  , b2 :: Bit
  , b3 :: Bit
  , b4 :: Bit
  , b5 :: Bit
  , b6 :: Bit
  , b7 :: Bit
  , b8 :: Bit
  , b9 :: Bit
  , b10 :: Bit
  , b11 :: Bit } deriving (Eq, Show)

defaultEntry :: DiagnosticEntry
defaultEntry = DiagnosticEntry Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero

fromBits :: [Bit] -> DiagnosticEntry
fromBits (a : b : c : d : e : f : g : h : i : j : k : l : _) = DiagnosticEntry a b c d e f g h i j k l
fromBits _ = defaultEntry

toDecimal :: DiagnosticEntry -> Int
toDecimal (DiagnosticEntry a b c d e f g h i j k l) =
  (a *** 2048) +
  (b *** 1024) +
  (c *** 512) +
  (d *** 256) +
  (e *** 128) +
  (f *** 64) +
  (g *** 32) +
  (h *** 16) +
  (i *** 8) +
  (j *** 4) +
  (k *** 2) +
  (l *** 1)

instance TextParsable DiagnosticEntry where
  fromText text = fromBits $ map (\x -> fromText x :: Bit) (map (\x -> [x]) text)

data BitCounter = BitCounter { onesCount :: Int, zeroesCount :: Int } deriving (Show, Eq)

updateCounter :: Bit -> BitCounter -> BitCounter
updateCounter Zero (BitCounter ones zeroes) = BitCounter ones (zeroes + 1)
updateCounter One (BitCounter ones zeroes) = BitCounter (ones + 1) zeroes

highest :: BitCounter -> Bit
highest (BitCounter ones zeroes)
  | ones >= zeroes = One
  | otherwise = Zero

lowest :: BitCounter -> Bit
lowest (BitCounter ones zeroes)
  | ones >= zeroes = Zero
  | otherwise = One

data DiagnosticsBitCounter =
  DiagnosticsBitCounter {
    b0Counter :: BitCounter
  , b1Counter :: BitCounter
  , b2Counter :: BitCounter
  , b3Counter :: BitCounter
  , b4Counter :: BitCounter
  , b5Counter :: BitCounter
  , b6Counter :: BitCounter
  , b7Counter :: BitCounter
  , b8Counter :: BitCounter
  , b9counter :: BitCounter
  , b10Counter :: BitCounter
  , b11Counter :: BitCounter
  } deriving (Eq, Show)

initialCounter :: DiagnosticsBitCounter
initialCounter = DiagnosticsBitCounter
  (BitCounter 0 0)
  (BitCounter 0 0)
  (BitCounter 0 0)
  (BitCounter 0 0)
  (BitCounter 0 0)
  (BitCounter 0 0)
  (BitCounter 0 0)
  (BitCounter 0 0)
  (BitCounter 0 0)
  (BitCounter 0 0)
  (BitCounter 0 0)
  (BitCounter 0 0)

updateBitsCounters :: DiagnosticsBitCounter -> DiagnosticEntry -> DiagnosticsBitCounter
updateBitsCounters
  (DiagnosticsBitCounter b0c b1c b2c b3c b4c b5c b6c b7c b8c b9c b10c b11c)
  (DiagnosticEntry bi0 bi1 bi2 bi3 bi4 bi5 bi6 bi7 bi8 bi9 bi10 bi11) =
  DiagnosticsBitCounter
    (updateCounter bi0 b0c)
    (updateCounter bi1 b1c)
    (updateCounter bi2 b2c)
    (updateCounter bi3 b3c)
    (updateCounter bi4 b4c)
    (updateCounter bi5 b5c)
    (updateCounter bi6 b6c)
    (updateCounter bi7 b7c)
    (updateCounter bi8 b8c)
    (updateCounter bi9 b9c)
    (updateCounter bi10 b10c)
    (updateCounter bi11 b11c)

fromDiagnostics :: [DiagnosticEntry] -> DiagnosticsBitCounter
fromDiagnostics entries = foldl updateBitsCounters initialCounter entries

highestBits :: DiagnosticsBitCounter -> DiagnosticEntry
highestBits (DiagnosticsBitCounter b0c b1c b2c b3c b4c b5c b6c b7c b8c b9c b10c b11c) =
  DiagnosticEntry
    (highest b0c)
    (highest b1c)
    (highest b2c)
    (highest b3c)
    (highest b4c)
    (highest b5c)
    (highest b6c)
    (highest b7c)
    (highest b8c)
    (highest b9c)
    (highest b10c)
    (highest b11c)

lowestBits :: DiagnosticsBitCounter -> DiagnosticEntry
lowestBits (DiagnosticsBitCounter b0c b1c b2c b3c b4c b5c b6c b7c b8c b9c b10c b11c) =
  DiagnosticEntry
    (lowest b0c)
    (lowest b1c)
    (lowest b2c)
    (lowest b3c)
    (lowest b4c)
    (lowest b5c)
    (lowest b6c)
    (lowest b7c)
    (lowest b8c)
    (lowest b9c)
    (lowest b10c)
    (lowest b11c)

gammaRate :: DiagnosticsBitCounter -> Int
gammaRate x = toDecimal $ highestBits x

epsilonRate :: DiagnosticsBitCounter -> Int
epsilonRate x = toDecimal $ lowestBits x

findLastMatchingFromLeft :: [DiagnosticEntry] -> [DiagnosticEntry -> Bit] -> ([DiagnosticEntry] -> DiagnosticEntry) -> DiagnosticEntry
findLastMatchingFromLeft (final : []) _ _ = final
findLastMatchingFromLeft entries (curr : remaining) target =
  findLastMatchingFromLeft (filter (\x -> (curr x) == (curr (target entries))) entries) remaining target
findLastMatchingFromLeft (first : _) [] _ = first
findLastMatchingFromLeft _ _ _ = defaultEntry

oxygenGeneratorRating :: [DiagnosticEntry] -> Int
oxygenGeneratorRating entries =
  toDecimal $ findLastMatchingFromLeft entries [b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11] (highestBits . fromDiagnostics)

co2ScrubberRating :: [DiagnosticEntry] -> Int
co2ScrubberRating entries =
  toDecimal $ findLastMatchingFromLeft entries [b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11] (lowestBits . fromDiagnostics)

powerConsumption :: DiagnosticsBitCounter -> Int
powerConsumption counter = (gammaRate counter) * (epsilonRate counter)

lifeSupportRating :: [DiagnosticEntry] -> Int
lifeSupportRating entries = (oxygenGeneratorRating entries) * (co2ScrubberRating entries)

powerConsumptionOutput :: FilePath -> IO ()
powerConsumptionOutput path = do
  diagnosticEntries <- parseLines path
  let diagnosticBitCounter = fromDiagnostics diagnosticEntries
  print $ "Power Consumption Rating... ==> " ++ show (powerConsumption diagnosticBitCounter)

lifeSupportRatingOutput :: FilePath -> IO ()
lifeSupportRatingOutput path = do
  diagnosticEntries <- parseLines path
  print $ "Life Support Rating... ==> " ++ show (lifeSupportRating diagnosticEntries)