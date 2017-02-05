(*
#load @"c:\Users\SVShah\Projects\fsaoc2016\5\FiveTest.fsx";; open FiveMain;; open FiveTest;;
*)
#load @"c:\Users\SVShah\Projects\fsaoc2016\5\FiveMain.fsx"
open FiveMain

(* 
  What i've learned:
  - this only has 64 elements
  - It has 16 f1's, then 16 f2's, etc.
  - It may be worthwhile to try out FSharpPlus
*)
let t1 = 
  let f1 i = i + 5
  let f2 i = i - 10
  let f3 i = i + 15
  let f4 i = i - 20
  seq {
    for i = 1 to 16 do yield f1
    for i = 1 to 16 do yield f2
    for i = 1 to 16 do yield f3
    for i = 1 to 16 do yield f4
  }

#r @"C:\Users\SVShah\Projects\fsaoc2016\packages\FSharpPlus\lib\net40\FSharpPlus.dll"
module FSPO = FSharpPlus.Operators
module FSPE = FSharpPlus.Extensions

let t2 = 
  [1;2;3;4]
  |> FSPO.map (List.replicate 3) 
  |> FSPO.concat
