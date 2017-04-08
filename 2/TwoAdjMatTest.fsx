#r @"./packages/FsCheck/lib/net452/FsCheck.dll"
//#load "./TwoCommon.fsx"
#load "./TwoAdjMat.fsx"

open FsCheck
open System
open TwoCommon
open TwoAdjMat

type AdjacencyMatrixTest () =
  static member AdjMatrixSquare9 () =
    sprintf "123%s456%s789" Environment.NewLine Environment.NewLine
    |> Board.parseBoard
    |> AdjacencyMatrix.create
    |> printfn "AMSquare9%s%A" Environment.NewLine
    true

  static member AdjMatrixDiamond () =
    sprintf
      "%s%s1%s%s234%s56789%s%sABC%s%s%sD"
      NOVERTEX NOVERTEX Environment.NewLine
      NOVERTEX Environment.NewLine
      Environment.NewLine
      NOVERTEX Environment.NewLine
      NOVERTEX NOVERTEX
    |> Board.parseBoard
    |> AdjacencyMatrix.create
    |> printfn "AMDiamond%s%A" Environment.NewLine
    true

(*
#load @"./TwoAdjMatTest.fsx"
let conf10k =
  { FsCheck.Config.Quick with MaxTest = 10000; MaxFail = 5000; Name = "10k" }
let conf1 = { FsCheck.Config.Quick with MaxTest = 1; MaxFail = 1; Name = "1" }
FsCheck.Check.All (conf1, typeof<TwoAdjMatTest.AdjacencyMatrixTest>)
*)
