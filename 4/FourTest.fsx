(*
#load @"./FourTest.fsx";; open FourMain;;
*)

#load @"./FourMain.fsx"
open FourMain

(** Tests for calculateChecksum **)

/// "ebbbccdddddddda"
let ccTest1 = calculateChecksum "ebbbccdddddddda" = "dbcae"
if ccTest1 then printfn "ccTest1: %b" ccTest1 else failwith "ccTest1"

/// "aaaaabbbzyx"
let ccTest2 = calculateChecksum "aaaaabbbzyx" = "abxyz"
if ccTest2 then printfn "ccTest2: %b" ccTest2 else failwith "ccTest2"

/// "abcdefgh"
let ccTest3 = calculateChecksum "abcdefgh" = "abcde"
if ccTest3 then printfn "ccTest3: %b" ccTest3 else failwith "ccTest3"

/// "notarealroom"
let ccTest4 = calculateChecksum "notarealroom" = "oarel"
if ccTest4 then printfn "ccTest4: %b" ccTest4 else failwith "ccTest4"

/// "totallyrealroom"
let ccTest5 = calculateChecksum "totallyrealroom" <> "decoy"
if ccTest5 then printfn "ccTest5: %b" ccTest5 else failwith "ccTest5"

let d4p1Test = day4part1 = 158835
if d4p1Test then printfn "d4p1Test: %b" d4p1Test else failwith "d4p1Test"

/// qzmt-zixmtkozy-ivhz-343 = very encrypted name
let scTest1 =
  @"qzmt-zixmtkozy-ivhz"
  |> Seq.map (fun elt -> shiftCipher elt 343)
  |> System.String.Concat
  |> (=) @"very encrypted name"
if scTest1 then printfn "scTest1: %b" scTest1 else failwith "scTest1"
