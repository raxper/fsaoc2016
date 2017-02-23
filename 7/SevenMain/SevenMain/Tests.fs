module Tests

open Expecto

[<Tests>]
let tests =
  testList "part1" [
    testCase "abba[mnop]qrst" <| fun _ ->
      let text = "abba[mnop]qrst"
      let funcAns = Util.supportsTLS text
      Expect.isTrue funcAns text

    testCase "abcd[bddb]xyyx" <| fun _ ->
      let text = "abcd[bddb]xyyx"
      let funcAns = Util.supportsTLS text
      Expect.isFalse funcAns text

    testCase "aaaa[qwer]tyui" <| fun _ ->
      let text = "aaaa[qwer]tyui"
      let funcAns = Util.supportsTLS text
      Expect.isFalse funcAns text

    testCase "ioxxoj[asdfgh]zxcvbn" <| fun _ ->
      let text = "ioxxoj[asdfgh]zxcvbn"
      let funcAns = Util.supportsTLS text
      Expect.isTrue funcAns text
  ]