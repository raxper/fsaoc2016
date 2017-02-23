module SevenMain

open Expecto

[<EntryPoint>]
let main argv =
    match argv with
    | [|first|] when first="p1" -> failwith "d7p1"
    | [|first|] when first="p2" -> failwith "d7p2"
    | _ -> Tests.runTestsInAssembly defaultConfig argv
