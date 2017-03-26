// Constants
let NO_VERTEX = "."

/// Direction
[<RequireQualifiedAccessAttribute>]
type Direction = | R | L | U | D
module Direction =
  let opposite =
    function | Direction.R -> Direction.L
             | Direction.L -> Direction.R
             | Direction.U -> Direction.D
             | Direction.D -> Direction.U

/// Vertex
type Vertex = | V of int * string with
  override x.ToString() = match x with | V(_, s) -> s
module Vertex =
  let create idx label = V(idx, label)
  let toInt = function | V(x, _) -> x
  let toString (x:Vertex) = x.ToString()
  let strToV lbl = Seq.find (fun (V(_, l)) -> l = lbl)
  let intToV idx = Seq.find (fun (V(i, _)) -> i = idx)

/// Edge
type Edge = | E of Vertex * Direction * Vertex with
  override x.ToString() =
    match x with E(v1, d, v2) -> sprintf "%A->%A->%A" v1 d v2
module Edge =
  let create dir (v1, v2) = E(v1, dir, v2)
  let toString (e:Edge) = e.ToString()

module Parser =
  let transpose rows =
    let numRowsO = Seq.length rows
    let numColsO = Seq.item 0 rows |> Seq.length
    seq {
      for i = 0 to numColsO-1 do
        yield seq {
          for j = 0 to numRowsO-1 do
            yield (Seq.item j rows |> Seq.item i)
        }
    }

  let validEdge (l1, l2) = l1 <> NO_VERTEX && l2 <> NO_VERTEX

  let potentialEdges verts rows =
    rows
    |> Seq.collect Seq.pairwise
    |> Seq.filter validEdge
    |> Seq.map
      (fun (vl1, vl2) -> Vertex.strToV vl1 verts, Vertex.strToV vl2 verts)

  let reverse (x, y) = y, x

  let squareup strs =
    seq {
      let longestRow = strs |> Seq.map Seq.length |> Seq.max
      for i = 0 to Seq.length strs-1 do
        let curStr = Seq.item i strs
        if Seq.length curStr = longestRow then
          yield curStr
        else
          yield (Seq.replicate (longestRow - Seq.length curStr) "."
                |> Seq.append curStr)
    }

  let parseBoard (strB:string)=
    let strs = strB.Split('\n') |> Seq.map (Seq.map string)
    let rows = squareup strs
    let cols = transpose rows

    let verts =
      rows |> Seq.concat |> Seq.filter ((<>) NO_VERTEX) |> Seq.mapi Vertex.create

    let redges = potentialEdges verts rows |> Seq.map (Edge.create Direction.R)
    let ledges = potentialEdges verts rows |> Seq.map reverse
                 |> Seq.map (Edge.create Direction.L)
    let dedges = potentialEdges verts cols |> Seq.map (Edge.create Direction.D)
    let uedges = potentialEdges verts cols |> Seq.map reverse
                 |> Seq.map (Edge.create Direction.U)
    let edges =
      Seq.append redges ledges
      |> Seq.append dedges
      |> Seq.append uedges

    verts, edges
