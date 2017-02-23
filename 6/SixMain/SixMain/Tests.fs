module Tests

open Expecto
open Solution

[<Tests>]
let testsPart1 =
  testList "part1" [
    testCase "easter" <| fun _ ->
      let answer = "easter"
      let funcAns = Solution.day6part1 @"..\SixMain\SixMain\test_input1.txt"
      Expect.isTrue (answer = funcAns) @"test input answer should be 'easter'"
  ]

[<Tests>]
let testsPart2 =
  testList "part2" [
    testCase "advent" <| fun _ -> 
      let answer = "advent"
      let funcAns = Solution.day6part2 @"..\SixMain\SixMain\test_input1.txt"
      Expect.isTrue (answer = funcAns) @"test input answer should be 'advent'"
  ]
