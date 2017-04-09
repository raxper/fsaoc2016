#load "./TwoCommon.fsx"
open TwoCommon

/// Edge list is a way to represent a graph by simply keeping an exhaustive list of all the edges.
module EdgeList =
  type EL =
    private | EL of Board with
    override m.ToString() =
      match m with
      | EL(b) ->
        sprintf "Edges: %A"
          (b.edges
          |> Seq.map
            (fun (E(v1,d,v2)) ->
              sprintf "%s%s%s" (v1.ToString()) (d.ToString()) (v2.ToString()))
          |> Seq.toList)

  /// Create a new instance, given edges and vertices.
  let create board = EL(board)

  /// Tries to get a destination Vertex based on a source Vertex and a Direction. If the connection exists, it returns the destination Vertex. Otherwise, it returns the source Vertex.
  let getNext el from dir =
    let b = match el with | EL(b) -> b
    let destv =
      Seq.tryFind
        (fun (E(v1,d,v2)) -> v1 = from && d = dir)
        b.edges
    match destv with
    | Some(E(_,_,v2)) -> v2
    | None -> from

(*
#load @"./TwoEdgeList.fsx"
open TwoEdgeList
/// Solution 1
TwoCommon.Solution.day2part1 @"./d2p1_map" @"./input_text" EdgeList.create EdgeList.getNext

/// Solution 2
TwoCommon.Solution.day2part1 @"./d2p2_map" @"./input_text" EdgeList.create EdgeList.getNext
*)