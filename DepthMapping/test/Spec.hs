
import Test.HUnit
import Lib

main :: IO ()
main =
  runTestTTAndExit $
    TestList [
      TestCase ( do
        let depths = [Depth 1, Depth 3, Depth 2, Depth 4]
        assertEqual ("counting single-window increases of " ++ show depths) 2 (countIncreasesSingleWindow depths)
      ),
      TestCase ( do
        let depths = [Depth 0, Depth 0, Depth 0, Depth 0]
        assertEqual ("counting single-window increases of " ++ show depths) 0 (countIncreasesSingleWindow depths)
      ),
      TestCase ( do
        let depths = [Depth 1, Depth 2]
        assertEqual ("counting single-window increases of " ++ show depths) 1 (countIncreasesSingleWindow depths)
      ),
      TestCase ( do
        let depths = [Depth 1, Depth 2, Depth 3]
        assertEqual ("counting single-window increases of " ++ show depths) 2 (countIncreasesSingleWindow depths)
      ),
      TestCase ( do
        let depths = [Depth 199, Depth 200, Depth 208, Depth 210, Depth 200, Depth 207, Depth 240, Depth 269, Depth 260, Depth 263]
        assertEqual ("counting single-window increases of " ++ show depths) 7 (countIncreasesSingleWindow depths)
      ),
      TestCase (do
        let depths = [Depth 1, Depth 3, Depth 2, Depth 5, Depth 1]
        assertEqual ("counting triple-window increases of " ++ show depths) 1 (countIncreasesTripleWindow depths)
      ),
      TestCase (do
        let depths = [Depth 199, Depth 200, Depth 208, Depth 210, Depth 200, Depth 207, Depth 240, Depth 269, Depth 260, Depth 263]
        assertEqual ("counting triple-window increases of " ++ show depths) 5 (countIncreasesTripleWindow depths)
      )
    ]
