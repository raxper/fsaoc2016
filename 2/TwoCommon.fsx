// Constants
let NOVERTEX = "."

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

type Board = {vertices:seq<Vertex>; edges:seq<Edge>}
module Board =
  // For Environment
  open System

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

  /// Helper function to identify a valid edge.
  let validEdge (l1, l2) = l1 <> NOVERTEX && l2 <> NOVERTEX

  let potentialEdgesByRow verts rows =
    rows
    |> Seq.collect Seq.pairwise
    |> Seq.filter validEdge
    |> Seq.map
      (fun (vl1, vl2) -> Vertex.strToV vl1 verts, Vertex.strToV vl2 verts)

  /// Reverses a 2-tuple (e.g. a pair of vertices).
  let reverse (x, y) = y, x

  /// Ensures that the board is a square and fills in blanks until it is. The parseBoard function requires a square board to work correctly.
  let squareup strs =
    seq {
      let longestRow = strs |> Seq.map Seq.length |> Seq.max
      for i = 0 to Seq.length strs-1 do
        let curStr = Seq.item i strs
        if Seq.length curStr = longestRow then
          yield curStr
        else
          yield (Seq.replicate (longestRow - Seq.length curStr) NOVERTEX
                |> Seq.append curStr)
    }

  let parseBoard (strB:string)=
    let strs = strB.Split([|Environment.NewLine|], StringSplitOptions.None)
               |> Seq.map (Seq.map string)
    let rows = squareup strs
    let cols = transpose rows

    let verts =
      rows |> Seq.concat |> Seq.filter ((<>) NOVERTEX) |> Seq.mapi Vertex.create

    let redges = potentialEdgesByRow verts rows
                 |> Seq.map (Edge.create Direction.R)
    let ledges = potentialEdgesByRow verts rows
                 |> Seq.map (reverse >> Edge.create Direction.L)
    let dedges = potentialEdgesByRow verts cols
                 |> Seq.map (Edge.create Direction.D)
    let uedges = potentialEdgesByRow verts cols
                 |> Seq.map (reverse >> Edge.create Direction.U)
    let edges =
      Seq.append redges ledges
      |> Seq.append dedges
      |> Seq.append uedges

    {vertices = verts; edges = edges}
