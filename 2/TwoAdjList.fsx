#load "./TwoCommon.fsx"
open TwoCommon

/// Adjacency list is a way to represent a graph by keeping a map from a vertex to all linked vertices.
module AdjacencyListDict =
  type AL =
    private
    | AL of System.Collections.Generic.IDictionary<Vertex, seq<Edge>> * Board
  with
    override m.ToString() =
      match m with
      | AL(d, _) ->
        seq {
          for k in d.Keys do
            yield (k.ToString() + "==>")
            for v in d.Item k do
              yield (v.ToString() + ", ")
            yield System.Environment.NewLine
        }
        |> Seq.reduce ( + )

  /// Create a new instance, given edges and vertices.
  let create board =
    let d =
      board.edges
      |> Seq.groupBy
        (fun x ->
          match x with
          | E(v1,_,_) -> v1)
      |> Seq.sortBy fst
      |> dict
    AL(d, board)

  /// Tries to get a destination Vertex based on a source Vertex and a Direction. If the connection exists, it returns the destination Vertex. Otherwise, it returns the source Vertex.
  let getNext (AL(d,_)) from dir : Vertex =
    let es = d.Item from
    match Seq.tryFind (fun (E(_,d,_)) -> dir = d) es with
    | Some (E(_,_,v2)) -> v2
    | None -> from

(*
#load @"./TwoAdjList.fsx"
open TwoAdjList
/// Solution 1
TwoCommon.Solution.day2part1 @"./d2p1_map" @"./input_text" AdjacencyListDict.create AdjacencyListDict.getNext

/// Solution 2
TwoCommon.Solution.day2part1 @"./d2p2_map" @"./input_text" AdjacencyListDict.create AdjacencyListDict.getNext
*)

/// Adjacency list is a way to represent a graph by keeping a map from a vertex to all linked vertices.
module AdjacencyListMap =
  type AL =
    private
    | AL of Map<Vertex, seq<Edge>> * Board
  with
    override m.ToString() =
      match m with
      | AL(d, _) ->
        Map.fold
          (fun acc k es ->
            acc + k.ToString() + "==>" +
            (Seq.fold (fun acc elt -> acc + elt.ToString() + ", ") "" es) +
            System.Environment.NewLine)
          ""
          d

  /// Create a new instance, given edges and vertices.
  let create board =
    let d =
      board.edges
      |> Seq.groupBy
        (fun x ->
          match x with
          | E(v1,_,_) -> v1)
      |> Seq.sortBy fst
      |> Map.ofSeq
    AL(d, board)

  /// Tries to get a destination Vertex based on a source Vertex and a Direction. If the connection exists, it returns the destination Vertex. Otherwise, it returns the source Vertex.
  let getNext (AL(d,_)) from dir =
    let es = Map.find from d
    match Seq.tryFind (fun (E(_,d,_)) -> dir = d) es with
    | Some (E(_,_,v2)) -> v2
    | None -> from
