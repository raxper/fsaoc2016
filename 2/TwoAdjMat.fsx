#r @"C:\Users\SVShah\odrive\Google Drive\Projects\fsaoc2016\2\TwoCommon.fsx"
open TwoCommon

/// Adjacency matrix as a square matrix.
module AdjacencyMatrix =
  type AdjMat =
    private | AM of Direction option [,] * Vertex list * Edge list with
    override m.ToString() = match m with | AM(x, _, _) -> sprintf "%A" x

  let create verts edges =
    let size = List.length verts
    AM(Array2D.create size size None, verts, edges)

  let addEdge from dir dest am =
    match am with
    | AM(y, vl, el) ->
      let x = Array2D.copy y
      Array2D.set x (Vertex.toInt from) (Vertex.toInt dest) (Some dir)
      AM(x, vl, el)

  let addEdges l am =
    List.fold (fun acc (E(v1, d, v2)) -> addEdge v1 d v2 acc) am l

  let getAdjIndex from dir am =
    match am with
    | AM(x) ->
      let fromInt = Vertex.toInt from
      let colBase = Array2D.base2 x
      x.[fromInt..fromInt,colBase..]
      |> Seq.cast<Direction option>
      |> Seq.tryFindIndex (( = ) (Some dir))
      |> function
        | Some idx -> idx + colBase
        | None -> fromInt

let tam1 = AdjacencyMatrix.createBased 1 9
printfn "created: %A" tam1

let v1, v2, v3, v4, v5, v6, v7, v8, v9 =
  Vertex.create 1 "1", Vertex.create 2 "2", Vertex.create 3 "3",
  Vertex.create 4 "4", Vertex.create 5 "5", Vertex.create 6 "6",
  Vertex.create 7 "7", Vertex.create 8 "8", Vertex.create 9 "9"

let edges =
  [E(v1, R, v4)]

let tam2 = AdjacencyMatrix.addEdge 1 R 2 tam1
printfn " added 1R2: %A" tam2
