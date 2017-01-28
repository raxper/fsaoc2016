(*
#load @"c:\Users\SVShah\Projects\fsaoc2016\4\FourTest.fsx";; open FourMain;;
*)

#load @"c:\Users\SVShah\Projects\fsaoc2016\4\FourMain.fsx"
open FourMain

(** Tests for calculateChecksum **)

/// "ebbbccdddddddda"
let ccTest1 = calculateChecksum "ebbbccdddddddda" = "dbcae"
printfn "ccTest1: %b" ccTest1

/// "aaaaabbbzyx"
let ccTest2 = calculateChecksum "aaaaabbbzyx" = "abxyz"
printfn "ccTest2: %b" ccTest2

/// "abcdefgh"
let ccTest3 = calculateChecksum "abcdefgh" = "abcde"
printfn "ccTest3: %b" ccTest3

/// "notarealroom"
let ccTest4 = calculateChecksum "notarealroom" = "oarel"
printfn "ccTest4: %b" ccTest4

/// "totallyrealroom"
let ccTest5 = calculateChecksum "totallyrealroom" <> "decoy"
printfn "ccTest5: %b" ccTest5
