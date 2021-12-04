module Lib where

import FileIO

data Bit = Zero | One

data DiagnosticEntry =
  DiagnosticEntry
  { sixteen :: Bit
  , eight :: Bit
  , four :: Bit
  , two :: Bit
  , one :: Bit
  }

data BitCounter = BitCounter { ones :: Int, zeroes :: Int }

data DiagnosticReport =
  DiagnosticReport {
    sixteens :: BitCounter
  , eights :: BitCounter
  , fours :: BitCounter
  , twos :: BitCounter
  , ones :: BitCounter
  }


fromBits :: [Bit] -> DiagnosticEntry
fromBits (a : b : c : d : e : _) = DiagnosticEntry a b c d e
fromBits _ = DiagnosticEntry Zero Zero Zero Zero Zero

instance TextParsable Bit where
  fromText text
    | text == "0" = Zero
    | text == "1" = One
    | otherwise = Zero

instance TextParsable DiagnosticEntry where
  fromText text = fromBits $ map (\x -> fromText x :: Bit) (words text)

