(*
#load @"c:\Users\SVShah\Projects\fsaoc2016\4\FourTest.fsx";; open FourMain;;
*)


(*
--- Day 4: Security Through Obscurity ---

Finally, you come across an information kiosk with a list of rooms. Of course, the list is encrypted and full of decoy data, but the instructions to decode the list are barely hidden nearby. Better remove the decoy data first.

Each room consists of an encrypted name (lowercase letters separated by dashes) followed by a dash, a sector ID, and a checksum in square brackets.

A room is real (not a decoy) if the checksum is the five most common letters in the encrypted name, in order, with ties broken by alphabetization. For example:

aaaaa-bbb-z-y-x-123[abxyz] is a real room because the most common letters are a (5), b (3), and then a tie between x, y, and z, which are listed alphabetically.
a-b-c-d-e-f-g-h-987[abcde] is a real room because although the letters are all tied (1 of each), the first five are listed alphabetically.
not-a-real-room-404[oarel] is a real room.
totally-real-room-200[decoy] is not.
Of the real rooms from the list above, the sum of their sector IDs is 1514.

What is the sum of the sector IDs of the real rooms?
*)

open System.IO
open System.Text.RegularExpressions

/// The puzzle input
let puzzleInput = @"4\puzzle_input.txt"

/// Given a string of characters, returns the checksum (top 5 characters by frequency; ties are broken by alphabetization.  Strategy is to first sort by letters, then by count.  Both sorts are stable, so sorting by count will retain alphabetization when counts are the same
let calculateChecksum (line: string) =
  line 
  |> Seq.countBy id
  |> Seq.sortBy fst
  |> Seq.sortByDescending snd
  |> Seq.truncate 5
  |> Seq.fold (fun accum (elt, _) -> accum + elt.ToString()) ""

let readFile () = 
  let lines = File.ReadLines puzzleInput |> Seq.cast |> Seq.map string
  let sectorIDs = 
    lines 
    |> Seq.map 
      (fun line -> (Regex.Match (line, @"[0-9]+")).Groups.[0].Value |> int)
  let calculatedChecksums = 
    lines
    |> Seq.map
      (fun line -> 
      (Regex.Matches (line, @"([a-zA-Z]+)\-")) 
      |> Seq.cast 
      |> Seq.map 
        (fun (x : System.Text.RegularExpressions.Match) -> 
        let substr = x.Value
        substr.Substring(0, String.length substr - 1))
      |> String.concat "" 
      |> calculateChecksum
      )
  let providedChecksums = 
    lines
    |> Seq.map
      (fun line -> (Regex.Match (line, @"\[([a-zA-Z]+)\]")).Groups.[1].Value)
  calculatedChecksums
  