module SixMain

open Expecto

[<EntryPoint>]
let main argv =
  match argv with
  | [|first|] when first = "p1" -> 
    let ans = Solution.day6part1 @"..\SixMain\SixMain\puzzle_input.txt"
    printfn "Day 6 Part 1: %A" ans
    0
  | [|first|] when first = "p2" ->
    let ans = Solution.day6part2 @"..\SixMain\SixMain\puzzle_input.txt"
    printfn "Day 6 Part 2: %A" ans
    0
  | _ -> Tests.runTestsInAssembly defaultConfig argv
