(*
#load @"c:\Users\SVShah\Projects\fsaoc2016\1\OneMain.fsx";; open OneMain;;
*)

open System.Text.RegularExpressions

type Direction = 
  | North 
  | South 
  | East 
  | West

type TurnTo = 
  | R
  | L

type Point = {
  x : int
  y : int
}

type Location = {
  dir : Direction
  pt : Point
}

let startPt = {
  x = 0
  y = 0
}

let startLoc = {
  dir = North
  pt = startPt
}

let toTurn = function 
  | 'R' -> R
  | 'L' -> L
  | _ -> failwith "Unknown turn type"

let turn way loc = 
  match way with
  | L -> 
    match loc.dir with
    | North -> { loc with dir = West }
    | West -> { loc with dir = South }
    | South -> { loc with dir = East }
    | East -> { loc with dir = North }
  | R ->
    match loc.dir with
    | North -> { loc with dir = East }
    | East -> { loc with dir = South } 
    | South -> { loc with dir = West }
    | West -> { loc with dir = North }

let advance dist loc = 
  match loc.dir with
  | North -> 
    let npt = { x = loc.pt.x; y = loc.pt.y + dist }
    { loc with pt = npt }
  | South -> 
    let npt = { x = loc.pt.x; y = loc.pt.y - dist }
    { loc with pt = npt }
  | West -> 
    let npt = { x = loc.pt.x - dist; y = loc.pt.y }
    { loc with pt = npt }
  | East -> 
    let npt = { x = loc.pt.x + dist; y = loc.pt.y }
    { loc with pt = npt }

let move way dist = (turn way) >> (advance dist)

let moveAll strList = 
  Regex.Matches (strList, @"[LR]\d+")
  |> Seq.cast
  |> Seq.fold 
    (fun oldloc (instr : System.Text.RegularExpressions.Match) -> 
      move 
        (instr.Value.[0] |> toTurn) 
        (instr.Value.Substring 1 |> int)
        oldloc
    )
    startLoc


let distance stloc endloc = (abs (stloc.x - endloc.x)) + (abs (stloc.y - endloc.y))

let partOne strList = 
  strList |> moveAll |> fun x -> distance startPt x.pt

let day1part1 = partOne "R2, L5, L4, L5, R4, R1, L4, R5, R3, R1, L1, L1, R4, L4, L1, R4, L4, R4, L3, R5, R4, R1, R3, L1, L1, R1, L2, R5, L4, L3, R1, L2, L2, R192, L3, R5, R48, R5, L2, R76, R4, R2, R1, L1, L5, L1, R185, L5, L1, R5, L4, R1, R3, L4, L3, R1, L5, R4, L4, R4, R5, L3, L1, L2, L4, L3, L4, R2, R2, L3, L5, R2, R5, L1, R1, L3, L5, L3, R4, L4, R3, L1, R5, L3, R2, R4, R2, L1, R3, L1, L3, L5, R4, R5, R2, R2, L5, L3, L1, L1, L5, L2, L3, R3, R3, L3, L4, L5, R2, L1, R1, R3, R4, L2, R1, L1, R3, R3, L4, L2, R5, R5, L1, R4, L5, L5, R1, L5, R4, R2, L1, L4, R1, L1, L1, L5, R3, R4, L2, R1, R2, R1, R1, R3, L5, R1, R4"

let inline tee fn x = x |> fn |> ignore; x

(*
let crossProduct p1 p2 = p1.x * p2.y - p2.x * p1.y

let doBoundingBoxesIntersect p1 p2 = 
  let a0 = fst p1
  let a1 = snd p1
  let b0 = fst p2
  let b1 = snd p2
  a0.x <= b1.x && a1.x >= b0.x && a0.y <= b1.y && a1.y >= b0.y

let isPointOnLine line pt =
  let st = fst line
  let ed = snd line
  let nLine = startPt, {x = ed.x-st.x; y = ed.y-st.y}
  let nPt = {x = pt.x - st.x; y = pt.y - st.y}
  let cp = crossProduct (snd nLine) nPt
  abs cp = 0

let isPointRightOfLine line pt =
  let st = fst line
  let ed = snd line
  let nLine = startPt, {x = ed.x - st.x; y = ed.y - st.y}
  let nPt = {x = pt.x - st.x; y = pt.y - st.y}
  crossProduct (snd nLine) nPt < 0

let lineSegmentTouchesOrCrossesLine l1 l2 =
  isPointOnLine l1 (fst l2)
    || isPointOnLine l1 (snd l2)
    || (isPointRightOfLine l1 (fst l2) <> isPointRightOfLine l1 (snd l2));

let getBoundingBox pt1 pt2 = 
  { x = min pt1.x pt2.x; y = min pt1.y pt2.y },
  { x = max pt1.x pt2.x; y = max pt1.y pt2.y }

let doLinesIntersect l1 l2 = 
  let bb1 = getBoundingBox (fst l1) (snd l1)
  let bb2 = getBoundingBox (fst l2) (snd l2)
  doBoundingBoxesIntersect bb1 bb2
    && lineSegmentTouchesOrCrossesLine bb1 bb2
    && lineSegmentTouchesOrCrossesLine bb2 bb1
*)

// https://martin-thoma.com/how-to-check-if-two-line-segments-intersect/
let doLinesIntersect2 l1 l2 =
  let A = fst l1
  let B = snd l1
  let O = fst l2
  let M = snd l2
  let v1 = {x = A.x - O.x; y = A.y - O.y}
  let v2 = {x = B.x - O.x; y = B.y - O.y}
  let v3 = {x = M.x - O.x; y = M.y - O.y}
  let v4 = {x = A.x - M.x; y = A.y - M.y}
  let v5 = {x = B.x - M.x; y = B.y - M.y}
  
  let v4 = {x = v4.x + v5.x; y = v4.y + v5.y}
  let v5 = {x = v1.x + v2.x; y = v1.y + v2.y}
  let dotProduct = v4.x * v5.x + v4.y * v5.y

  if dotProduct > 0 then false
  else (
    let crossV1V3 = v1.x*v3.y - v1.y*v3.x
    let crossV3V2 = v3.x*v2.y - v3.y*v2.x
    (crossV1V3 < 0) = (crossV3V2 < 0)
  )

let intersectionPoint l1 l2 = 
  let x1 = (fst l1).x
  let y1 = (fst l1).y
  let x2 = (snd l1).x
  let y2 = (snd l1).y
  let x3 = (fst l2).x
  let y3 = (fst l2).y
  let x4 = (snd l2).x
  let y4 = (snd l2).y

  let denom = (x1-x2)*(y3-y4)-(y1-y2)*(x3-x4)
  {x = ((x1*y2-y1*x2)*(x3-x4)-(x1-x2)*(x3*y4-y3*x4))/denom;
  y = ((x1*y2-y1*x2)*(y3-y4)-(y1-y2)*(x3*y4-y3*x4))/denom}

let dupFinder strList =
  Regex.Matches (strList, @"[LR]\d+")
  |> Seq.cast
  |> Seq.scan
    (fun oldloc (instr : System.Text.RegularExpressions.Match) -> 
      move 
        (instr.Value.[0] |> toTurn) 
        (instr.Value.Substring 1 |> int)
        oldloc
    )
    startLoc
  |> Seq.map (fun x -> x.pt)
  // Define lines 
  |> Seq.pairwise
  |> Seq.fold 
    (fun accum elt ->
      if snd accum = None then
        let s = fst accum
        if Seq.exists (doLinesIntersect2 elt) s then
          let ip = Seq.find (doLinesIntersect2 elt) s
          s, intersectionPoint ip elt |> Some
        else s @ [elt], None
      else accum 
    )
    ([], None)
  |> snd
  |> fun x -> 
    match x with
    | None -> failwith "No intersection"
    | Some y -> y
  |> distance startPt 

let day1part2 = dupFinder "R2, L5, L4, L5, R4, R1, L4, R5, R3, R1, L1, L1, R4, L4, L1, R4, L4, R4, L3, R5, R4, R1, R3, L1, L1, R1, L2, R5, L4, L3, R1, L2, L2, R192, L3, R5, R48, R5, L2, R76, R4, R2, R1, L1, L5, L1, R185, L5, L1, R5, L4, R1, R3, L4, L3, R1, L5, R4, L4, R4, R5, L3, L1, L2, L4, L3, L4, R2, R2, L3, L5, R2, R5, L1, R1, L3, L5, L3, R4, L4, R3, L1, R5, L3, R2, R4, R2, L1, R3, L1, L3, L5, R4, R5, R2, R2, L5, L3, L1, L1, L5, L2, L3, R3, R3, L3, L4, L5, R2, L1, R1, R3, R4, L2, R1, L1, R3, R3, L4, L2, R5, R5, L1, R4, L5, L5, R1, L5, R4, R2, L1, L4, R1, L1, L1, L5, R3, R4, L2, R1, R2, R1, R1, R3, L5, R1, R4"
