module SevenMain

open Expecto
open Expecto.ExpectoFsCheck

[<EntryPoint>]
let main argv =
  match argv with
  | [|first|] when first="p1" ->
    let ans = Util.day7part1 @"..\SevenMain\SevenMain\puzzle_input.txt"
    printfn "Day 7 Part 1: %A" ans
    0
  | [|first|] when first="p2" -> 
    let ans = Util.day7part2 @"..\SevenMain\SevenMain\puzzle_input.txt"
    printfn "Day 7 Part 2: %A" ans
    0
  | _ -> Tests.runTestsInAssembly defaultConfig argv
