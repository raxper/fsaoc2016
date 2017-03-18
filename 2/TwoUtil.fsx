open System.Text.RegularExpressions

/// Direction - kept public for ease of use (pattern matching).
type Direction = | R | L | U | D
module Direction =
  let opposite = function | R -> L | L -> R | U -> D | D -> U

/// Vertex - kept public for ease of use (pattern matching)
type Vertex = | V of int * string with
  override x.ToString() = let V(_, s) = x in sprintf "%A" s
module Vertex =
  let create idx label = V(idx, label)
  let toInt = function | V(x, _) -> x

/// Edge - kept public for ease of use (pattern matching)
type Edge = | E of Vertex * Direction * Vertex with
  override x.ToString() = let E(v1, d, v2) = x in sprintf "%A, %A, %A" v1 d v2
module Edge =
  let create v1 dir v2 = E(v1, dir, v2)
  
/// Parse a string and return a list of vertices and edges.
let parseBoard strB =
  
  failwith "parseBoard"

/// Adjacency matrix as a square matrix.
module AdjacencyMatrix =
  type AdjMat = 
    private | AM of Direction option [,] * Vertex list * Edge list with
    override m.ToString() = match m with | AM(x, _, _) -> sprintf "%A" x

  let createFrom2DBoard strBoard = 
    let size = List.length verts
    AM(Array2D.create size size None, verts, edges)

  /// Warning: NOT functional, mutates the state of the underlying Array2D
  let addEdge from dir dest am =
    match am with
    | AM(x) ->
      Array2D.set x (Vertex.toInt from) (Vertex.toInt dest) (Some dir)
      am

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
