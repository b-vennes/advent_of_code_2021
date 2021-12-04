import Test.HUnit

import Lib
import FileIO

main :: IO ()
main =
  runTestTTAndExit $ TestList [
    TestCase $ assertEqual "Zero *** 5" 0 $ Zero *** 5,
    TestCase $ assertEqual "One *** 111" 111 $ One *** 111,
    TestCase $ assertEqual "from text '1'" One $ fromText "1",
    TestCase $ assertEqual "from text '0'" Zero $ fromText "0",
    TestCase $
      assertEqual
        "from bits [Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero]"
        (DiagnosticEntry Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero)
        (fromBits [Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero]),
    TestCase $
      assertEqual
        "from bits [One, Zero, One, Zero, One, Zero, One, Zero, One, Zero, One, Zero]"
        (DiagnosticEntry One Zero One Zero One Zero One Zero One Zero One Zero)
        (fromBits [One, Zero, One, Zero, One, Zero, One, Zero, One, Zero, One, Zero]),
    TestCase $
      assertEqual
        "to decimal (DiagnosticEntry Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero)"
        0
        (toDecimal $ DiagnosticEntry Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero),
    TestCase $
      assertEqual
        "to decimal (DiagnosticEntry One Zero Zero One Zero Zero One Zero Zero One Zero Zero)"
        (2048 + 256 + 32 + 4)
        (toDecimal $ DiagnosticEntry One Zero Zero One Zero Zero One Zero Zero One Zero Zero),
    TestCase $
      assertEqual
        "from text '000100001000'"
        (DiagnosticEntry Zero Zero Zero One Zero Zero Zero Zero One Zero Zero Zero)
        (fromText "000100001000"),
    TestCase $
      assertEqual
        "update counter (BitCounter 11 155) with Zero"
        (BitCounter 11 156)
        (updateCounter Zero $ BitCounter 11 155),
    TestCase $
      assertEqual
        "update counter (BitCounter 4 33) with One"
        (BitCounter 5 33)
        (updateCounter One $ BitCounter 4 33),
    TestCase $
      assertEqual
        "highest of (BitCounter 44 77)"
        Zero
        (highest $ BitCounter 44 77),
    TestCase $
      assertEqual
        "highest of (BitCounter 33 1)"
        One
        (highest $ BitCounter 33 1),
    TestCase $
      assertEqual
        "lowest of (BitCounter 10 6)"
        Zero
        (lowest $ BitCounter 10 6),
    TestCase $
      assertEqual
        "lowest of (BitCounter 44 1000)"
        One
        (lowest $ BitCounter 4 1000),
    TestCase $
      assertEqual
        ("update bit counters (DiagnosticBitCounter (0 0) (1 4) (5 5) (3 2) (9 9) (3 4) (0 1) (3 4) (5 5) (2 3) (4 3) (2 2))" ++
        " with (DiagnosticEntry One One Zero One One Zero Zero One One Zero One One)")
        (DiagnosticsBitCounter
          (BitCounter 1 0)
          (BitCounter 2 4)
          (BitCounter 5 6)
          (BitCounter 4 2)
          (BitCounter 10 9)
          (BitCounter 3 5)
          (BitCounter 0 2)
          (BitCounter 4 4)
          (BitCounter 6 5)
          (BitCounter 2 4)
          (BitCounter 5 3)
          (BitCounter 3 2))
        (updateBitsCounters
          (DiagnosticsBitCounter
            (BitCounter 0 0)
            (BitCounter 1 4)
            (BitCounter 5 5)
            (BitCounter 3 2)
            (BitCounter 9 9)
            (BitCounter 3 4)
            (BitCounter 0 1)
            (BitCounter 3 4)
            (BitCounter 5 5)
            (BitCounter 2 3)
            (BitCounter 4 3)
            (BitCounter 2 2))
          (DiagnosticEntry One One Zero One One Zero Zero One One Zero One One)),
    TestCase $
      assertEqual
        ("from diagnostcs [ (DiagnosticEntry One One One One One One One One One One One One), " ++
        "(DiagnosticEntry Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero) ]")
        (DiagnosticsBitCounter
          (BitCounter 1 1)
          (BitCounter 1 1)
          (BitCounter 1 1)
          (BitCounter 1 1)
          (BitCounter 1 1)
          (BitCounter 1 1)
          (BitCounter 1 1)
          (BitCounter 1 1)
          (BitCounter 1 1)
          (BitCounter 1 1)
          (BitCounter 1 1)
          (BitCounter 1 1))
        (fromDiagnostics $ [
          DiagnosticEntry One One One One One One One One One One One One,
          DiagnosticEntry Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero
        ]),
    TestCase $
      assertEqual
        "highest bits of (DiagnosticsBitCounter (1 3) (3 7) (44 3) (23 2) (9 88) (10 3) (12 5) (15 3) (20 2) (8 9) (3 4) (4 5))"
        (DiagnosticEntry Zero Zero One One Zero One One One One Zero Zero Zero)
        (highestBits $ DiagnosticsBitCounter
          (BitCounter 1 3)
          (BitCounter 3 7)
          (BitCounter 44 3)
          (BitCounter 23 2)
          (BitCounter 9 88)
          (BitCounter 10 3)
          (BitCounter 12 5)
          (BitCounter 15 3)
          (BitCounter 20 2)
          (BitCounter 8 9)
          (BitCounter 3 4)
          (BitCounter 4 5)),
    TestCase $
      assertEqual
        "lowest bits of (DiagnosticsBitCounter (2 9) (4 5) (4 5) (30 1) (45 6) (64 22) (100 90) (1 78) (3 32) (4 19) (8 10) (22 5))"
        (DiagnosticEntry One One One Zero Zero Zero Zero One One One One Zero)
        (lowestBits $ DiagnosticsBitCounter
          (BitCounter 2 9)
          (BitCounter 4 5)
          (BitCounter 4 5)
          (BitCounter 30 1)
          (BitCounter 45 6)
          (BitCounter 64 22)
          (BitCounter 100 90)
          (BitCounter 1 78)
          (BitCounter 3 32)
          (BitCounter 4 19)
          (BitCounter 8 10)
          (BitCounter 22 5)),
    TestCase $
      assertEqual
        ("find last matching from left for (DiagnosticEntry One One Zero One One Zero Zero Zero One One Zero Zero) with entries " ++
        "[(DiagnosticEntry One One Zero Zero One One Zero Zero One One One Zero), (DiagnosticEntry One One Zero One One Zero Zero Zero Zero One One Zero)]" ++
        "(DiagnosticEntry Zero One Zero Zero One One Zero Zero One One One Zero), (DiagnosticEntry One Zero Zero Zero One One Zero Zero One One One Zero)")
        (DiagnosticEntry One One Zero One One Zero Zero Zero Zero One One Zero)
        (findLastMatchingFromLeft
          (DiagnosticEntry One One Zero One One Zero Zero Zero One One Zero Zero)
          [ (DiagnosticEntry One One Zero Zero One One Zero Zero One One One Zero)
          , (DiagnosticEntry One One Zero One One Zero Zero Zero Zero One One Zero)
          , (DiagnosticEntry Zero One Zero Zero One One Zero Zero One One One Zero)
          , (DiagnosticEntry One Zero Zero Zero One One Zero Zero One One One Zero) ]
          [b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11]),
    TestCase (do
      let diagnosticEntries = map fromText ["111111100100", "111111111110", "111111110110", "111111110111", "111111110101", "111111101111", "111111100111", "111111110000", "111111111001", "111111100010", "111111101010"]
      assertEqual
        ("CO2 Scrubber Rating of (DiagnosticsBitCounter (12 0) (12 0) (12 0) (12 0) (12 0) (12 0) (12 0) (7 5) (5 7) (9 3) (7 5) (5 7)) " ++
        "for [111111100100, 111111111110, 111111110110, 111111110111, 111111110101, 111111101111, 111111100111, 111111110000, 111111111001, 111111100010, 111111101010]")
        (10)
        (co2ScrubberRating (fromDiagnostics diagnosticEntries) diagnosticEntries)),
    TestCase (do
      let diagnosticEntries = map fromText ["000000000100", "000000011110", "000000010110", "000000010111", "000000010101", "000000001111", "000000000111", "000000010000", "000000011001", "000000000010", "000000001010"]
      assertEqual
        ("oxygen generator rating of (DiagnosticsBitCounter (0 12) (0 12) (0 12) (0 12) (0 12) (0 12) (0 12) (7 5) (5 7) (9 3) (7 5) (5 7)) " ++
        "for [ 000000000100, 000000011110, 000000010110, 000000010111, 000000010101, 000000001111, 000000000111, 000000010000, 000000011001, " ++
        "000000000010, 000000001010]")
        (23)
        (oxygenGeneratorRating (fromDiagnostics diagnosticEntries) diagnosticEntries))
  ]
