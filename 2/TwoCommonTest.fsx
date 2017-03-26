#r @"./packages/FsCheck/lib/net452/FsCheck.dll"
#load "./TwoCommon.fsx"

open FsCheck
open TwoCommon
open System

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
  static member TWVLHelper s1 s2 s3 s4 s5 =
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
        |> Board.transpose
        |> Seq.length
      )

  static member TransposeWidthVsLength s1 s2 s3 s4 s5 =
    (not (isNull s1) && not (isNull s2) && not (isNull s3) && not (isNull s4) &&  not (isNull s5))
      ==> (BoardTest.TWVLHelper s1 s2 s3 s4 s5)

(*
let conf = { Config.Quick with MaxTest = 10000; MaxFail = 5000; Name = "10k" }
Check.All (conf, typeof<DirectionTest>)
Check.All (conf, typeof<VertexTest>)
Check.All (conf, typeof<EdgeTest>)
Check.All (conf, typeof<ParserTest>)
*)
