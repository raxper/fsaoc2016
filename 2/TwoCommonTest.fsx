#r @"./packages/FsCheck/lib/net452/FsCheck.dll"
#load "./TwoCommon.fsx"
#load "./TwoAdjMat.fsx"
#load "./TwoEdgeList.fsx"
#load "./TwoAdjList.fsx"
#load "./TwoIndGraph.fsx"

open FsCheck
open TwoCommon
open System
open TwoAdjMat
open TwoEdgeList
open TwoAdjList
open TwoIndGraph
open System.Diagnostics

type DirectionTest () =
  static member OppositeDirIsDiff (dir:Direction) =
    Direction.opposite dir <> dir

  static member OppositesMatch (dir:Direction) =
    Direction.opposite dir = Direction.opposite dir

  static member OppositeOppositeDirIsSame (dir:Direction) =
    (Direction.opposite dir |> Direction.opposite) = dir

type VertexTest () =
  static member Create (i:int) (l:string) =
    (not (isNull l)) ==> (V(i, l) = Vertex.create i l)

  static member ToInt (i:int) (l:string) =
    let v = Vertex.create i l
    (not (isNull l)) ==> (i = Vertex.toInt v)

  static member ToString (i:int) (l:string) =
    let v = Vertex.create i l
    (not (isNull l)) ==> (l = Vertex.toString v)

  static member StrToV (i:int) (l:string) =
    let v = Vertex.create i l
    let sv = Seq.singleton v
    (not (isNull l)) ==> (v = Vertex.strToV l sv)

  static member IntToV (i:int) (l:string) =
    let v = Vertex.create i l
    let sv = Seq.singleton v
    (not (isNull l)) ==> (v = Vertex.intToV i sv)

type EdgeTest () =
  static member Create (i1, l1) (i2, l2) (d:Direction) =
    let v1 = V(i1, l1)
    let v2 = V(i2, l2)
    (not (isNull l1) && not (isNull l2))
      ==> (E(v1, d, v2) = Edge.create d (v1, v2))

  static member ToString (i1, l1) (i2, l2) (d:Direction) =
    let v1 = V(i1, l1)
    let v2 = V(i2, l2)
    let e = E(v1, d, v2)
    (not (isNull l1) && not (isNull l2))
      ==> (Edge.toString e = sprintf "%A->%A->%A" v1 d v2)



type BoardTest () =
  static member TransposeWidthVsLength s1 s2 s3 s4 s5 =
    let tee f x = f x |> ignore; x
    let TWVLHelper s1 s2 s3 s4 s5 =
      let s =
        sprintf "%s%s%s%s%s%s%s%s%s"
          s1 Environment.NewLine s2 Environment.NewLine s3 Environment.NewLine
          s4 Environment.NewLine s5
      let longest =
        [s1; s2; s3; s4; s5]
        |> Seq.map Seq.length
        |> Seq.max
      longest =
        (
          s.Split([|Environment.NewLine|], StringSplitOptions.None)
          |> Seq.map (Seq.map string)
          |> Board.squareup
          |> Board.transpose
          |> Seq.length
        )
    let c1 =
      not (isNull s1) && not (isNull s2) && not (isNull s3)
      && not (isNull s4) &&  not (isNull s5)
    c1 ==> (lazy (TWVLHelper s1 s2 s3 s4 s5))

type AdjacencyMatrixTest () =
  // static member AdjMatrixSquare9 () =
  //   sprintf "123%s456%s789" Environment.NewLine Environment.NewLine
  //   |> Board.parseBoard
  //   |> AdjacencyMatrix.create
  //   |> printfn "AMSquare9%s%A" Environment.NewLine
  //   true

  // static member AdjMatrixDiamond () =
  //   sprintf
  //     "%s%s1%s%s234%s56789%s%sABC%s%s%sD"
  //     NOVERTEX NOVERTEX Environment.NewLine
  //     NOVERTEX Environment.NewLine
  //     Environment.NewLine
  //     NOVERTEX Environment.NewLine
  //     NOVERTEX NOVERTEX
  //   |> Board.parseBoard
  //   |> AdjacencyMatrix.create
  //   |> printfn "AMDiamond%s%A" Environment.NewLine
  //   true

  static member AMD2P1Test () =
    Solution.day2part1
      @"./d2p1_map" @"./input_text_test1"
      AdjacencyMatrix.create AdjacencyMatrix.getNext
      = "1985"

  static member AMD2P2Test () =
    Solution.day2part1
      @"./d2p2_map" @"./input_text_test1"
      AdjacencyMatrix.create AdjacencyMatrix.getNext
      = "5DB3"

  static member AMD2P1Solution () =
    Solution.day2part1
      @"./d2p1_map" @"./input_text"
      AdjacencyMatrix.create AdjacencyMatrix.getNext
      = "97289"

  static member AMD2P2Solution () =
    Solution.day2part1
      @"./d2p2_map" @"./input_text"
      AdjacencyMatrix.create AdjacencyMatrix.getNext
      = "9A7DC"

type EdgeListTest =
  // static member EdgeListSquare9 () =
  //   sprintf "123%s456%s789" Environment.NewLine Environment.NewLine
  //   |> Board.parseBoard
  //   |> EdgeList.create
  //   |> printfn "ELSquare9%s%A" Environment.NewLine
  //   true

  // static member EdgeListDiamond () =
  //   sprintf
  //     "%s%s1%s%s234%s56789%s%sABC%s%s%sD"
  //     NOVERTEX NOVERTEX Environment.NewLine
  //     NOVERTEX Environment.NewLine
  //     Environment.NewLine
  //     NOVERTEX Environment.NewLine
  //     NOVERTEX NOVERTEX
  //   |> Board.parseBoard
  //   |> EdgeList.create
  //   |> printfn "ELDiamond%s%A" Environment.NewLine
  //   true

  static member ELD2P1Test () =
    Solution.day2part1
      @"./d2p1_map" @"./input_text_test1"
      EdgeList.create EdgeList.getNext
      = "1985"

  static member ELD2P2Test () =
    Solution.day2part1
      @"./d2p2_map" @"./input_text_test1"
      EdgeList.create EdgeList.getNext
      = "5DB3"

  static member ELD2P1Solution () =
    Solution.day2part1
      @"./d2p1_map" @"./input_text"
      EdgeList.create EdgeList.getNext
      = "97289"

  static member ELD2P2Solution () =
    Solution.day2part1
      @"./d2p2_map" @"./input_text"
      EdgeList.create EdgeList.getNext
      = "9A7DC"

type AdjacencyListDictTest =
  // static member AdjacencyListSquare9 () =
  //   sprintf "123%s456%s789" Environment.NewLine Environment.NewLine
  //   |> Board.parseBoard
  //   |> AdjacencyListDict.create
  //   |> printfn "ELSquare9%s%A" Environment.NewLine
  //   true

  // static member AdjacencyListDiamond () =
  //   sprintf
  //     "%s%s1%s%s234%s56789%s%sABC%s%s%sD"
  //     NOVERTEX NOVERTEX Environment.NewLine
  //     NOVERTEX Environment.NewLine
  //     Environment.NewLine
  //     NOVERTEX Environment.NewLine
  //     NOVERTEX NOVERTEX
  //   |> Board.parseBoard
  //   |> AdjacencyListDict.create
  //   |> printfn "ELDiamond%s%A" Environment.NewLine
  //   true

  static member ALDD2P1Test () =
    Solution.day2part1
      @"./d2p1_map" @"./input_text_test1"
      AdjacencyListDict.create AdjacencyListDict.getNext
      = "1985"

  static member ALDD2P2Test () =
    Solution.day2part1
      @"./d2p2_map" @"./input_text_test1"
      AdjacencyListDict.create AdjacencyListDict.getNext
      = "5DB3"

  static member ALDD2P1Solution () =
    Solution.day2part1
      @"./d2p1_map" @"./input_text"
      AdjacencyListDict.create AdjacencyListDict.getNext
      = "97289"

  static member ALDD2P2Solution () =
    Solution.day2part1
      @"./d2p2_map" @"./input_text"
      AdjacencyListDict.create AdjacencyListDict.getNext
      = "9A7DC"

type AdjacencyListMapTest =
  // static member AdjacencyListSquare9 () =
  //   sprintf "123%s456%s789" Environment.NewLine Environment.NewLine
  //   |> Board.parseBoard
  //   |> AdjacencyListMap.create
  //   |> printfn "ELSquare9%s%A" Environment.NewLine
  //   true

  // static member AdjacencyListDiamond () =
  //   sprintf
  //     "%s%s1%s%s234%s56789%s%sABC%s%s%sD"
  //     NOVERTEX NOVERTEX Environment.NewLine
  //     NOVERTEX Environment.NewLine
  //     Environment.NewLine
  //     NOVERTEX Environment.NewLine
  //     NOVERTEX NOVERTEX
  //   |> Board.parseBoard
  //   |> AdjacencyListMap.create
  //   |> printfn "ELDiamond%s%A" Environment.NewLine
  //   true

  static member ALMD2P1Test () =
    Solution.day2part1
      @"./d2p1_map" @"./input_text_test1"
      AdjacencyListMap.create AdjacencyListMap.getNext
      = "1985"

  static member ALMD2P2Test () =
    Solution.day2part1
      @"./d2p2_map" @"./input_text_test1"
      AdjacencyListMap.create AdjacencyListMap.getNext
      = "5DB3"

  static member ALMD2P1Solution () =
    Solution.day2part1
      @"./d2p1_map" @"./input_text"
      AdjacencyListMap.create AdjacencyListMap.getNext
      = "97289"

  static member ALMD2P2Solution () =
    Solution.day2part1
      @"./d2p2_map" @"./input_text"
      AdjacencyListMap.create AdjacencyListMap.getNext
      = "9A7DC"

type InductiveGraphTest =
  // static member InductiveGraphSquare9 () =
  //   sprintf "123%s456%s789" Environment.NewLine Environment.NewLine
  //   |> Board.parseBoard
  //   |> InductiveGraph.create
  //   |> printfn "IGSquare9%s%A" Environment.NewLine
  //   true

  // static member InductiveGraphDiamond () =
  //   sprintf
  //     "%s%s1%s%s234%s56789%s%sABC%s%s%sD"
  //     NOVERTEX NOVERTEX Environment.NewLine
  //     NOVERTEX Environment.NewLine
  //     Environment.NewLine
  //     NOVERTEX Environment.NewLine
  //     NOVERTEX NOVERTEX
  //   |> Board.parseBoard
  //   |> InductiveGraph.create
  //   |> printfn "IGDiamond%s%A" Environment.NewLine
  //   true

  static member IGD2P1Test () =
    Solution.day2part1
      @"./d2p1_map" @"./input_text_test1"
      InductiveGraph.create InductiveGraph.getNext
      = "1985"

  static member IGD2P2Test () =
    Solution.day2part1
      @"./d2p2_map" @"./input_text_test1"
      InductiveGraph.create InductiveGraph.getNext
      = "5DB3"

  static member IGD2P1Solution () =
    Solution.day2part1
      @"./d2p1_map" @"./input_text"
      InductiveGraph.create InductiveGraph.getNext
      = "97289"

  static member IGD2P2Solution () =
    Solution.day2part1
      @"./d2p2_map" @"./input_text"
      InductiveGraph.create InductiveGraph.getNext
      = "9A7DC"

(*
#load @"./TwoCommonTest.fsx"
let conf = { FsCheck.Config.Quick with MaxTest = 1000; MaxFail = 500; Name = "1k" }
let conf1 = { FsCheck.Config.Quick with MaxTest = 100; MaxFail = 1; Name = "1" }

FsCheck.Check.All (conf, typeof<TwoCommonTest.DirectionTest>)
FsCheck.Check.All (conf, typeof<TwoCommonTest.VertexTest>)
FsCheck.Check.All (conf, typeof<TwoCommonTest.EdgeTest>)
FsCheck.Check.All (conf, typeof<TwoCommonTest.BoardTest>)

let amtimer = System.Diagnostics.Stopwatch()
amtimer.Start()
FsCheck.Check.All (conf1, typeof<TwoCommonTest.AdjacencyMatrixTest>)
amtimer.Stop()

let eltimer = System.Diagnostics.Stopwatch()
eltimer.Start()
FsCheck.Check.All (conf1, typeof<TwoCommonTest.EdgeListTest>)
eltimer.Stop()

let aldtimer = System.Diagnostics.Stopwatch()
aldtimer.Start()
FsCheck.Check.All (conf1, typeof<TwoCommonTest.AdjacencyListDictTest>)
aldtimer.Stop()

let almtimer = System.Diagnostics.Stopwatch()
almtimer.Start()
FsCheck.Check.All (conf1, typeof<TwoCommonTest.AdjacencyListMapTest>)
almtimer.Stop()

let igtimer = System.Diagnostics.Stopwatch()
igtimer.Start()
FsCheck.Check.All (conf1, typeof<TwoCommonTest.InductiveGraphTest>)
igtimer.Stop()

printfn "Adjacency Matrix: %s" (amtimer.Elapsed.ToString())
printfn "Edge List: %s" (eltimer.Elapsed.ToString())
printfn "Adjancency List Dict: %s" (aldtimer.Elapsed.ToString())
printfn "Adjancency List Map: %s" (almtimer.Elapsed.ToString())
printfn "Inductive Graph: %s" (igtimer.Elapsed.ToString())
*)
