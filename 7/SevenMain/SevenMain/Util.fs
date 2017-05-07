module Util

open System.IO
open System.Text.RegularExpressions

/// For debugging
let tee f x = f x |> ignore; x

/// Extract supernet sequences from an IP address.
let supernetSeq ip =
  Regex.Replace (ip, @"\[\w+\]", " ")
  |> fun s -> s.Split ([|' '|])

/// Extract hypernet sequences from an IP address.
let hypernetSeq ip =
  Regex.Matches (ip, @"\[\w+\]")
  |> Seq.cast<Match>
  |> Seq.map (fun m -> Regex.Match(m.Value, @"\w+"))
  |> Seq.cast<Match>
  |> Seq.map (fun x -> x.Value)

/// Checks if a string contains a four-character sequence which consists of a pair of two different characters followed by the reverse of that pair
let isABBA (str:string) =
  str
  |> Seq.windowed 4
  |> Seq.exists (fun s -> s.[0] = s.[3] && s.[1] = s.[2] && s.[0] <> s.[1])

/// checks if a string supports TLS, which means that it has one ABBA and it has no ABBAs in the hypernet sequences.
let supportsTLS (str:string) : bool =
  (supernetSeq str |> Seq.exists isABBA)
  && (hypernetSeq str |> Seq.exists isABBA |> not)

/// Day 7 Part 1
let day7part1 filename =
  File.ReadAllLines filename
  |> Seq.filter supportsTLS
  |> Seq.length

/// An ABA pattern is any three-character sequence which consists of the same character twice with a different character between them, such as xyx or aba.
let hasABA (str:string) =
  str
  |> Seq.windowed 3
  |> Seq.exists (fun x -> x.[0] = x.[2] && x.[0] <> x.[1])

/// Converts a 3-character ABA to a BAB.
let flipABA (str:string) = sprintf "%c%c%c" str.[1] str.[0] str.[1]

/// Get all ABAs patterns in a string. Runs on one supernet string at a time.
let getABAs (str:string) =
  str
  |> Seq.windowed 3
  |> Seq.map (System.String)
  |> Seq.filter hasABA

/// A BAB corresponds to an ABA and is the same characters but in reversed positions: yxy and bab, respectively.
let hasBAB abas hnStr =
  let babs =
    hnStr
    |> getABAs
    |> Set.ofSeq
  abas
  |> Set.ofSeq
  |> Set.map flipABA
  |> Set.intersect babs
  |> (Set.isEmpty >> not)

/// An IP supports SSL if it has an Area-Broadcast Accessor, or ABA, anywhere in the supernet sequences (outside any square bracketed sections), and a corresponding Byte Allocation Block, or BAB, anywhere in the hypernet sequences.
let supportsSSL line =
  let abas =
    supernetSeq line
    |> Seq.collect getABAs
  (supernetSeq line |> Seq.exists hasABA)
  && (hypernetSeq line |> Seq.exists (hasBAB abas))

let day7part2 filename =
  File.ReadAllLines filename
  |> Seq.filter supportsSSL
  |> Seq.length
