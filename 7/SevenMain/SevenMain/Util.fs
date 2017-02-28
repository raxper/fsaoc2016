module Util

open System.IO
open System.Text.RegularExpressions

/// For debugging
let tee f x = f x |> ignore; x

/// Checks if a string contains a four-character sequence which consists of a pair of two different characters followed by the reverse of that pair
let isABBA (str:string) =
  str 
  |> Seq.windowed 4 
  |> Seq.exists (fun s -> s.[0] = s.[3] && s.[1] = s.[2] && s.[0] <> s.[1]) 

/// checks if a string supports TLS, which means that it has one ABBA and it has no ABBAs in the hypernet sequences.
let supportsTLS (str:string) : bool = 
  let outerStrings = 
    Regex.Replace (str, @"\[\w+\]", " ")
    |> fun s -> s.Split ([|' '|])
  let hypernetStrings = 
    Regex.Matches (str, @"\[\w+\]")
    |> Seq.cast
    |> Seq.map (fun (m:Match) -> Regex.Match(m.Value, @"\w+"))
    |> Seq.cast<Match>
    |> Seq.map (fun x -> x.Value)
  (Seq.exists isABBA outerStrings) && (Seq.exists isABBA hypernetStrings |> not)

/// Day 7 Part 1
let day7part1 filename =
  File.ReadAllLines filename
  |> Seq.filter supportsTLS
  |> Seq.length

/// An ABA is any three-character sequence which consists of the same character twice with a different character between them, such as xyx or aba. This function's parameter is a single supernet string.
let isABA (str:string) = 
  str
  |> Seq.windowed 3
  |> Seq.exists (fun x -> x.[0] = x.[2] && x.[0] <> x.[1])

/// Converts a 3-character ABA to a BAB.
let convertABAtoBAB (str:string) = sprintf "%c%c%c" str.[1] str.[0] str.[1]

/// A BAB corresponds to an ABA and is the same characters but in reversed positions: yxy and bab, respectively.
let isBAB abas str = 
  let babs = 
    str
    |> Seq.windowed 3
    |> Seq.filter (fun x -> x.[0] = x.[2] && x.[0] <> x.[1])
    |> Seq.map (System.String)
    |> Set.ofSeq
  abas
  |> Set.map convertABAtoBAB
  |> Set.intersect babs
  |> Set.count
  |> ( < ) 0

/// Get all ABAs in a string. Runs on one supernet string at a time.
let getABAs (str:string) = 
  str
  |> Seq.windowed 3
  |> Seq.map (System.String)
  |> Seq.filter isABA

/// An IP supports SSL if it has an Area-Broadcast Accessor, or ABA, anywhere in the supernet sequences (outside any square bracketed sections), and a corresponding Byte Allocation Block, or BAB, anywhere in the hypernet sequences.
let supportsSSL line = 
  let supernetSeq = 
    Regex.Replace (line, @"\[\w+\]", " ")
    |> fun s -> s.Split ([|' '|])
    |> Set.ofArray
  let hypernetSeq =
    Regex.Matches (line, @"\[\w+\]")
    |> Seq.cast
    |> Seq.map (fun (m:Match) -> Regex.Match(m.Value, @"\w+"))
    |> Seq.cast<Match>
    |> Seq.map (fun x -> x.Value)
    |> Set.ofSeq
  let abas =
    supernetSeq
    |> Seq.map getABAs
    |> Seq.concat
    |> Set.ofSeq
  (Seq.exists isABA supernetSeq) && (Seq.exists (isBAB abas) hypernetSeq)

let day7part2 filename =
  File.ReadAllLines filename
  |> Seq.filter supportsSSL
  |> Seq.length
