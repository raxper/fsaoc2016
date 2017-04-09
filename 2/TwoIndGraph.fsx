#r @"./packages/Aether/lib/net45/Aether.dll"
#r @"./packages/Hekate/lib/net45/Hekate.dll"
#load @"./TwoCommon.fsx"

open TwoCommon
open Hekate

/// Inductive graph is a functional representation of a graph.
module InductiveGraph =
  type IG =
    private | IG of Graph<Vertex, string, Direction> * Board with
    override m.ToString() =
      match m with
      | IG(g, b) -> sprintf "Inductive Graph:%s%A" System.Environment.NewLine g

  /// Create a new instance, given edges and vertices.
  let create board =
    Graph.create
      (board.vertices
      |> Seq.map (fun x -> x, Vertex.toString x)
      |> Seq.toList)
      (board.edges
      |> Seq.map (fun x -> x |> Edge.fromV,
                           x |> Edge.toV,
                           x |> Edge.direction)
      |> Seq.toList)

  /// Tries to get a destination Vertex based on a source Vertex and a Direction. If the connection exists, it returns the destination Vertex. Otherwise, it returns the source Vertex.
  let getNext ig from dir =
    match Graph.Nodes.successors from ig with
    | None -> from
    | Some succs ->
      succs
      |> List.tryFind (fun (_,d) -> d = dir)
      |> function
        | None -> from
        | Some (dest,_) -> dest

(*
#load @"./TwoIndGraph.fsx"
open TwoIndGraph
/// Solution 1
Solution.day2part1 @"./d2p1_map" @"./input_text" asdf.create asdf.getNext

/// Solution 2
Solution.day2part1 @"./d2p2_map" @"./input_text" asdf.create asdf.getNext
*)