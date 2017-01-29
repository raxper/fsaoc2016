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

type Line = {
  line: string
  sectorID: int
  calculatedChecksum: string
  providedChecksum: string
  decryptedText: string
}

let emptyLine = {
  line = ""; sectorID = 0; calculatedChecksum = ""; providedChecksum = "";
  decryptedText = ""
}

/// Given a string of characters, returns the checksum (top 5 characters by frequency; ties are broken by alphabetization.  Strategy is to first sort by letters, then by count.  Both sorts are stable, so sorting by count will retain alphabetization when counts are the same
let calculateChecksum (line: string) =
  line 
  |> Seq.countBy id
  |> Seq.sortBy fst
  |> Seq.sortByDescending snd
  |> Seq.truncate 5
  |> Seq.fold (fun accum (elt, _) -> accum + elt.ToString()) ""

let lines filename = 
  File.ReadAllLines filename 
  |> Seq.cast 
  |> Seq.map (fun x -> {emptyLine with line = string x})

let extractSectorIDs lines = 
  lines 
  |> Seq.map 
    (fun l -> 
    {l with 
      sectorID = (Regex.Match (l.line, @"[0-9]+")).Groups.[0].Value |> int})

let calculateChecksums lines = 
  lines
  |> Seq.map
    (fun l -> 
      {l with 
        calculatedChecksum = 
        (Regex.Matches (l.line, @"([a-zA-Z]+)\-")) 
        |> Seq.cast 
        |> Seq.map 
          (fun (x : System.Text.RegularExpressions.Match) -> 
            let substr = x.Value
            substr.Substring(0, String.length substr - 1))
        |> String.concat "" 
        |> calculateChecksum
      }
    )

let extractProvidedChecksums lines = 
  lines
  |> Seq.map
    (fun l -> 
      {l with 
        providedChecksum = 
        (Regex.Match (l.line, @"\[([a-zA-Z]+)\]")).Groups.[1].Value})

let day4part1 =
  lines puzzleInput
  |> extractSectorIDs
  |> extractProvidedChecksums
  |> calculateChecksums
  |> Seq.filter (fun x -> x.providedChecksum = x.calculatedChecksum)
  |> Seq.fold (fun accum elt -> accum + elt.sectorID) 0

(*
--- Part Two ---

With all the decoy data out of the way, it's time to decrypt this list and get moving.

The room names are encrypted by a state-of-the-art shift cipher, which is nearly unbreakable without the right software. However, the information kiosk designers at Easter Bunny HQ were not expecting to deal with a master cryptographer like yourself.

To decrypt a room name, rotate each letter forward through the alphabet a number of times equal to the room's sector ID. A becomes B, B becomes C, Z becomes A, and so on. Dashes become spaces.

For example, the real name for qzmt-zixmtkozy-ivhz-343 is very encrypted name.

What is the sector ID of the room where North Pole objects are stored?
*)

/// Decrypts a single letter using the sector ID 
let shiftCipher (letter: char) shift =
  let asciiStart = 97
  let numChars = 26
  let charToInt x = (int x) - asciiStart
  let intToChar x = char (x + asciiStart)
  if letter = '-' then ' '
  else letter |> charToInt |> (+) shift |> (fun x -> x % numChars) |> intToChar

let day4part2 = 
  lines puzzleInput
  |> extractSectorIDs
  |> extractProvidedChecksums
  |> calculateChecksums
  |> Seq.filter (fun x -> x.calculatedChecksum = x.providedChecksum)
  |> Seq.map
    (fun l ->
    {l with 
      decryptedText = 
      (Regex.Match (l.line, @"([a-zA-Z]+\-)+")).Groups.[0].Value
      |> Seq.map (fun elt -> shiftCipher elt l.sectorID)
      |> System.String.Concat
    })
  |> Seq.filter (fun l -> l.decryptedText.StartsWith("north"))
