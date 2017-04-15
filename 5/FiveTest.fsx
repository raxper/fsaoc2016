(*
#load @"./FiveTest.fsx";;
#load @"./FiveMain.fsx";;
open FiveMain;;
open FiveTest;;
*)
#load @"./FiveMain.fsx"
open FiveMain

let md5ootb (msg: string) =
  use md5 = System.Security.Cryptography.MD5.Create()
  msg
  |> System.Text.Encoding.ASCII.GetBytes
  |> md5.ComputeHash
  |> Seq.map (fun c -> c.ToString("X2"))
  |> Seq.reduce ( + )

let md5test1 =
  let str = ""
  let x = md5sum str
  let y = md5ootb str
  if x = y then printfn "md5sum %s: %s" str x; true
  else failwith (sprintf "md5test1 \"%s\": %s" str x)

let md5test2 =
  let str = "a"
  let x = md5sum str
  let y = md5ootb str
  if x = y then printfn "md5sum %s: %s" str x; true
  else failwith (sprintf "md5test1 \"%s\": %s" str x)

let md5test3 =
  let str = "abc"
  let x = md5sum str
  let y = md5ootb str
  if x = y then printfn "md5sum %s: %s" str x; true
  else failwith (sprintf "md5test1 \"%s\": %s" str x)

let md5test4 =
  let str = "message digest"
  let x = md5sum str
  let y = md5ootb str
  if x = y then printfn "md5sum %s: %s" str x; true
  else failwith (sprintf "md5test1 \"%s\": %s" str x)

let md5test5 =
  let str = "abcdefghijklmnopqrstuvwxyz"
  let x = md5sum str
  let y = md5ootb str
  if x = y then printfn "md5sum %s: %s" str x; true
  else failwith (sprintf "md5test1 \"%s\": %s" str x)

let md5test6 =
  let str = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
  let x = md5sum str
  let y = md5ootb str
  if x = y then printfn "md5sum %s: %s" str x; true
  else failwith (sprintf "md5test1 \"%s\": %s" str x)

let md5test7 =
  let str = "12345678901234567890123456789012345678901234567890123456789012345678901234567890"
  let x = md5sum str
  let y = md5ootb str
  if x = y then printfn "md5sum %s: %s" str x; true
  else failwith (sprintf "md5test1 \"%s\": %s" str x)
