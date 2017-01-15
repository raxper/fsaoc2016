(*
#load @"c:\Users\SVShah\Projects\fsaoc2016\3\ThreeMain.fsx";; open ThreeMain;;
*)

(*
--- Day 3: Squares With Three Sides ---
http://adventofcode.com/2016/day/3 
*)

(*
Now that you can think clearly, you move deeper into the labyrinth of hallways and office furniture that makes up this part of Easter Bunny HQ. This must be a graphic design department; the walls are covered in specifications for triangles.

Or are they?

The design document gives the side lengths of each triangle it describes, but... 5 10 25? Some of these aren't triangles. You can't help but mark the impossible ones.

In a valid triangle, the sum of any two sides must be larger than the remaining side. For example, the "triangle" given above is impossible, because 5 + 10 is not larger than 25.

In your puzzle input, how many of the listed triangles are possible?
*)

open System.IO
open System.Text.RegularExpressions

(* Applies comparison to one possible triangle's sides *)
let possibleTriangle sOne sTwo sThree : bool =
  let triangleComparison longerSide shorterSides = longerSide < shorterSides
  let inOrderOfLength = [sOne; sTwo; sThree] |> List.sort
  let sumOfShorter = inOrderOfLength.[0] + inOrderOfLength.[1]
  let longestSide = inOrderOfLength.[2]
  triangleComparison longestSide sumOfShorter 

(* Batch driver to evaluate input file *)
let possibleTrianglesCount triangleEvaluator = 
  File.ReadLines @"3\puzzle_input"
  |> Seq.map
    ((fun line -> Regex.Matches (line, @"[0-9]+"))
    >> Seq.cast
    >> Seq.map
      (fun (x : System.Text.RegularExpressions.Match) -> x.Value |> int)
    >> Seq.toList
    )
  |> Seq.filter (fun il -> triangleEvaluator il.[0] il.[1] il.[2])

(* Answer for Day 3 Part 1 *)
let day3part1 = 
  possibleTrianglesCount possibleTriangle
  |> Seq.length

(*
--- Part Two ---

Now that you've helpfully marked up their design documents, it occurs to you that triangles are specified in groups of three vertically. Each set of three numbers in a column specifies a triangle. Rows are unrelated.

For example, given the following specification, numbers with the same hundreds digit would be part of the same triangle:

101 301 501
102 302 502
103 303 503
201 401 601
202 402 602
203 403 603
In your puzzle input, and instead reading by columns, how many of the listed triangles are possible?
*)

