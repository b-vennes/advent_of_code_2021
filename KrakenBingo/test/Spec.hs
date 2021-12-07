import Lib

main :: IO ()
main = do
  print (
    "card marked with 1: " ++ show (
      mark
        1
        (BingoCards [
          BingoCard [
            (True, 0), (False, 1), (True, 2), (True, 3), (True, 4),
            (False, 5), (False, 6), (False, 7), (True, 8), (False, 9),
            (True, 10), (False, 11), (False, 12), (True, 13), (True, 14),
            (False, 15), (True, 16), (True, 17), (False, 18), (False, 19),
            (False, 20), (True, 21), (False, 22), (False, 23), (False, 24)
          ] ] ) ) )
  print (
    "find winning score: " ++ show (
      findWinningScore
        (Numbers [1])
        (BingoCards [
          BingoCard [
            (True, 0), (False, 1), (True, 2), (True, 3), (True, 4),
            (False, 5), (False, 6), (False, 7), (True, 8), (False, 9),
            (True, 10), (False, 11), (False, 12), (True, 13), (True, 14),
            (False, 15), (True, 16), (True, 17), (False, 18), (False, 19),
            (False, 20), (True, 21), (False, 22), (False, 23), (False, 24)
          ] ] ) ) )
--  print $ "winner: " ++
--    show (winner $
--      BingoCard [
--        (True, 0), (True, 1), (True, 2), (True, 3), (True, 4),
--        (False, 5), (False, 6), (False, 7), (True, 8), (False, 9),
--        (True, 10), (False, 11), (False, 12), (True, 13), (True, 14),
--        (False, 15), (True, 16), (True, 17), (False, 18), (False, 19),
--        (False, 20), (True, 21), (False, 22), (False, 23), (False, 24)
--      ])
--  print $ "row: " ++
--    show ( row 0 $
--      BingoCard [
--        (True, 0), (True, 1), (True, 2), (True, 3), (True, 4),
--        (False, 5), (False, 6), (False, 7), (True, 8), (False, 9),
--        (True, 10), (False, 11), (False, 12), (True, 13), (True, 14),
--        (False, 15), (True, 16), (True, 17), (False, 18), (False, 19),
--        (False, 20), (True, 21), (False, 22), (False, 23), (False, 24)
--      ] )
--  print $ "all marked: " ++
--    show (allMarked $ row 0 $
--      BingoCard [
--        (True, 0), (True, 1), (True, 2), (True, 3), (True, 4),
--        (False, 5), (False, 6), (False, 7), (True, 8), (False, 9),
--        (True, 10), (False, 11), (False, 12), (True, 13), (True, 14),
--        (False, 15), (True, 16), (True, 17), (False, 18), (False, 19),
--        (False, 20), (True, 21), (False, 22), (False, 23), (False, 24)
--      ] )
--  print $ "find winner: " ++
--    show (
--      findWinner
--        1
--        (BingoCards [
--          BingoCard [
--            (True, 0), (True, 1), (True, 2), (True, 3), (True, 4),
--            (False, 5), (False, 6), (False, 7), (True, 8), (False, 9),
--            (True, 10), (False, 11), (False, 12), (True, 13), (True, 14),
--            (False, 15), (True, 16), (True, 17), (False, 18), (False, 19),
--            (False, 20), (True, 21), (False, 22), (False, 23), (False, 24)
--          ] ] ) )

