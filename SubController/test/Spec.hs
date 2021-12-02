import Test.HUnit
import Lib

main :: IO ()
main = runTestTTAndExit $ TestList [
  TestCase (do
    let sub = SubPositionSimple 0 0
    assertEqual "move simple sub forward 2 from (0,0)" (SubPositionSimple 2 0) (forward 2 sub)
  ),
  TestCase (do
    let sub = SubPositionSimple (-1) 5
    assertEqual "move simple sub forward 4 from (-1,5)" (SubPositionSimple 3 5) (forward 4 sub)
  ),
  TestCase (do
    let sub = SubPositionSimple 0 0
    assertEqual "move simple sub down 3 from (0,0)" (SubPositionSimple 0 (3)) (down 3 sub)
  ),
  TestCase (do
   let sub = SubPositionSimple 0 0
   assertEqual "move simple sub up 10 from (0,0)" (SubPositionSimple 0 (-10)) (up 10 sub)
  ),
  TestCase (do
    let sub = SubPositionSimple 0 0
    let course = [Forward 5, Down 5, Forward 8, Up 3, Down 8, Forward 2]
    assertEqual "follow course [F5, D5, F8, U3, D8, F2] from simple sub (0,0)" (SubPositionSimple 15 10) $ followCourse course sub
  ),
  TestCase (do
    assertEqual "hash of sub position (9,5)" 45 $ hash $ SubPositionSimple 9 5
  ),
  TestCase (do
    result <- parseCourse "TestInput.txt"
    assertEqual "parse course in TestInput.txt" [Forward 1, Up 100, Down 20000, Up 100, Forward (-1), Down 3] result
  ),
  TestCase (do
    let sub = SubPositionAimed 0 0 0
    assertEqual "move aimed sub up 3" (SubPositionAimed 0 0 (-3)) (up 3 sub)
  ),
  TestCase (do
    let sub = SubPositionAimed 0 0 0
    assertEqual "move aimed sub down 5" (SubPositionAimed 0 0 5) (down 5 sub)
  ),
  TestCase (do
    let sub = SubPositionAimed 0 0 2
    assertEqual "move aimed sub forward 3 from (0, 0, 2)" (SubPositionAimed 3 6 2) (forward 3 sub)
  )]
