module Tests

open Expecto
open FsCheck
open Expecto.ExpectoFsCheck 

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

    testCase "aaaa[qwer]tyui[qwer]tyui" <| fun _ ->
      let text = "aaaa[qwer]tyui[qwer]tyui"
      let funcAns = Util.supportsTLS text
      Expect.isFalse funcAns text

    testCase "ioxxoj[asdfgh]zxcvbn" <| fun _ ->
      let text = "ioxxoj[asdfgh]zxcvbn"
      let funcAns = Util.supportsTLS text
      Expect.isTrue funcAns text

    testCase "ioxxoj[asdfasdf]zxcvbn[asdfasdf]zxcvbn" <| fun _ ->
      let text = "ioxxoj[asdfasdf]zxcvbn[asdfasdf]zxcvbn"
      let funcAns = Util.supportsTLS text
      Expect.isTrue funcAns text
  ]


[<Tests>]
let tests2 =
  testList "part2" [
    testCase "aba[bab]xyz" <| fun _ ->
      let text = "aba[bab]xyz"
      let funcAns = Util.supportsSSL text
      Expect.isTrue funcAns text

    testCase "xyx[xyx]xyx" <| fun _ ->
      let text = "xyx[xyx]xyx"
      let funcAns = Util.supportsSSL text
      Expect.isFalse funcAns text

    testCase "aaa[kek]eke" <| fun _ ->
      let text = "aaa[kek]eke"
      let funcAns = Util.supportsSSL text
      Expect.isTrue funcAns text

    testCase "zazbz[bzb]cdb" <| fun _ ->
      let text = "zazbz[bzb]cdb"
      let funcAns = Util.supportsSSL text
      Expect.isTrue funcAns text

    testCase "xyx[xyx]xyx[xyx]xyx" <| fun _ ->
      let text = "xyx[xyx]xyx[xyx]xyx"
      let funcAns = Util.supportsSSL text
      Expect.isFalse funcAns text

    testCase "zazbz[bzb]cdb[bzb]cdb" <| fun _ ->
      let text = "zazbz[bzb]cdb[bzb]cdb"
      let funcAns = Util.supportsSSL text
      Expect.isTrue funcAns text
  ]
