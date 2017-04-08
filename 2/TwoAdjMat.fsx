#load "./TwoCommon.fsx"
open TwoCommon

/// Adjacency matrix as a square matrix. Row = "from", column = "to". The index of a Vertex in the Vertex list matches its index in the 2D array that backs this data structure.
module AdjacencyMatrix =
  type AdjMat =
    private | AM of Direction option [,] * Board with
    override m.ToString() =
      match m with
      | AM(x, b) ->
        "  " +
        (Seq.map Vertex.toString b.vertices
        |> Seq.fold (sprintf "%s%s") "") +
        (seq {
          for i = 0 to Seq.length b.vertices - 1 do
            yield
              sprintf "%s%s " System.Environment.NewLine
                ((Seq.item i b.vertices).ToString())
            yield
              x.[i..i,0..]
              |> Seq.cast<Direction option>
              |> Seq.fold
                (fun accum d ->
                  match d with
                  | Some dir -> accum + (sprintf "%A" dir)
                  | None -> accum + NOVERTEX
                ) ""
        }
        |> Seq.fold ( + ) "")

  /// Add an edge to the adjacency matrix.
  let addEdge e am =
    match e, am with
    | E(from, dir, dest), AM(y, b) ->
      let x = Array2D.copy y
      Array2D.set x (Vertex.toInt from) (Vertex.toInt dest) (Some dir)
      AM(x, b)

  /// Add a listof edges to the adjacency matrix.
  let addEdges b am =
    Seq.fold (fun acc e -> addEdge e acc) am b.edges

  /// Create a new adjacency matrix, given a list of vertices and edges.
  let create board =
    let size = Seq.length board.vertices
    AM(Array2D.create size size None, board)
    |> addEdges board

  /// Tries to get a destination Vertex based on a source Vertex and a Direction. If the connection exists, it returns the destination Vertex. Otherwise, it returns the source Vertex.
  let getNext am from dir =
    match am with
    | AM(x, b) ->
      let fromInt = Vertex.toInt from
      let colBase = Array2D.base2 x
      x.[fromInt..fromInt,colBase..]
      |> Seq.cast<Direction option>
      |> Seq.tryFindIndex (( = ) (Some dir))
      |> function
        | Some idx -> Seq.item idx b.vertices
        | None -> from

module TwoAdjMatSolution =
  open System.Text.RegularExpressions
  open System.IO

  let moveAll func start instr = V(0, "0")

  let day2part1 boardFile instrFile =
    let board =
      seq {
        let lines = File.ReadAllLines boardFile
        for i = 0 to Seq.length lines - 1 do
          yield Seq.item i lines
          yield System.Environment.NewLine
      }
      |> Seq.fold ( + ) ""
      |> Board.parseBoard
    let am = AdjacencyMatrix.create board
    File.ReadAllLines instrFile
    |> Seq.map
      (moveAll (AdjacencyMatrix.getNext am) (Vertex.strToV "5" board.vertices))
    |> Seq.map Vertex.toString
    |> Seq.reduce ( + )
