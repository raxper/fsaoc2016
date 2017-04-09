#load "./TwoCommon.fsx"
open TwoCommon

/// Inductive graph is a way to represent a graph functionally.
module InductiveGraph =
  // type EL =
    // private | EL of Board with
    // override m.ToString() =
    //   match m with
    //   | EL(b) ->
    //     sprintf "Vertices: %A%sEdges: %A"
    //       (b.vertices |> Seq.toList)
    //       System.Environment.NewLine
    //       (b.edges |> Seq.toList)

  /// Add an edge.
  let addEdge e am =
    failwith "addEdge"

  /// Add a list of edges.
  let addEdges b am =
    failwith "addEdges"

  /// Create a new instance, given edges and vertices.
  let create board =
    failwith "create"

  /// Tries to get a destination Vertex based on a source Vertex and a Direction. If the connection exists, it returns the destination Vertex. Otherwise, it returns the source Vertex.
  let getNext am from dir =
    failwith "getNext"

(*
#load @"./asdf.fsx"
open asdf
/// Solution 1
Solution.day2part1 @"./d2p1_map" @"./input_text" asdf.create asdf.getNext

/// Solution 2
Solution.day2part1 @"./d2p2_map" @"./input_text" asdf.create asdf.getNext
*)