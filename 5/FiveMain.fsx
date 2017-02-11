(*
#load @"c:\Users\SVShah\Projects\fsaoc2016\5\FiveTest.fsx";; 
#load @"c:\Users\SVShah\Projects\fsaoc2016\5\FiveMain.fsx";; 
open FiveMain;; 
open FiveTest;;
*)

(*
--- Day 5: How About a Nice Game of Chess? ---

You are faced with a security door designed by Easter Bunny engineers that seem to have acquired most of their security knowledge by watching hacking movies.

The eight-character password for the door is generated one character at a time by finding the MD5 hash of some Door ID (your puzzle input) and an increasing integer index (starting with 0).

A hash indicates the next character in the password if its hexadecimal representation starts with five zeroes. If it does, the sixth character in the hash is the next character of the password.

For example, if the Door ID is abc:

The first index which produces a hash that starts with five zeroes is 3231929, which we find by hashing abc3231929; the sixth character of the hash, and thus the first character of the password, is 1.
5017308 produces the next interesting hash, which starts with 000008f82..., so the second character of the password is 8.
The third time a hash starts with five zeroes is for abc5278568, discovering the character f.
In this example, after continuing this search a total of eight times, the password is 18f47a30.

Given the actual Door ID, what is the password?

Your puzzle input is cxdnnyjw.
*)

#r @"C:\Users\SVShah\Projects\fsaoc2016\packages\FSharpx.Collections\lib\net40\FSharpx.Collections.dll"
open FSharpx.Collections

// MD5 algorithm
// https://tools.ietf.org/html/rfc1321 
// https://en.wikipedia.org/wiki/MD5#Algorithm 
// https://rosettacode.org/wiki/MD5/Implementation 

// /////////////////////////////////////////////////////////////////////////////
// Define Functions
// /////////////////////////////////////////////////////////////////////////////

(*
funF, funG, funH, funI :: Fun
funF x y z = (x .&. y) .|. (complement x .&. z)
funG x y z = (x .&. z) .|. (complement z .&. y)
funH x y z = x `xor` y `xor` z
funI x y z = y `xor` (complement z .|. x)
*)
let funF x y z : uint32 = (x &&& y) ||| (~~~x &&& z)
let funG x y z : uint32 = (x &&& z) ||| (~~~z &&& y)
let funH x y z : uint32 = x ^^^ y ^^^ z
let funI x y z : uint32 = y ^^^ (~~~z ||| x)

(* 
idxF, idxG, idxH, idxI :: Int -> Int
idxF i = i
idxG i = (5 * i + 1) `mod` 16
idxH i = (3 * i + 5) `mod` 16
idxI i = 7 * i `mod` 16
*)
let idxF i : int = i
let idxG i = (5 * i + 1) % 16
let idxH i = (3 * i + 5) % 16
let idxI i = 7 * i % 16

// /////////////////////////////////////////////////////////////////////////////
// Define Arrays
// /////////////////////////////////////////////////////////////////////////////

(* 
funA :: Array Int Fun
funA = listArray (1,64) $ replicate 16 =<< [funF, funG, funH, funI]
 
idxA :: Array Int Int
idxA = listArray (1,64) $ zipWith ($) (replicate 16 =<< [idxF, idxG, idxH, idxI]) [0..63]
 
rotA :: Array Int Int
rotA = listArray (1,64) $ concat . replicate 4 =<<
       [[7, 12, 17, 22], [5, 9, 14, 20], [4, 11, 16, 23], [6, 10, 15, 21]]
 
sinA :: Array Int Word32
sinA = listArray (1,64) $ map (floor . ( * mult ) . abs . sin) [1..64]
    where mult = 2 ** 32 :: Double
*)
let funA : (uint32 -> uint32 -> uint32 -> uint32) list = 
  [funF; funG; funH; funI]
  |> List.collect (List.replicate 16)

let idxA =
  [idxF; idxG; idxH; idxI]
  |> List.collect (List.replicate 16)
  |> List.map2 (fun idx func -> func idx) [0..63]

let rotA : int list =
  [[7; 12; 17; 22]; [5; 9; 14; 20]; [4; 11; 16; 23]; [6; 10; 15; 21]]
  |> List.collect (List.replicate 4)
  |> List.concat

/// floor(2^32 × abs(sin(i + 1)))
let sinA =
  [1. .. 64.]
  |> List.map (sin >> abs >> (( * ) (2.**32.)) >> floor >> uint32)

// /////////////////////////////////////////////////////////////////////////////
// Define Logic
// /////////////////////////////////////////////////////////////////////////////

(*
data MD5 = MD5
    { a :: {-# UNPACK #-} !Word32
    , b :: {-# UNPACK #-} !Word32
    , c :: {-# UNPACK #-} !Word32
    , d :: {-# UNPACK #-} !Word32
    }
*)
type MD5 = 
  {
    a : uint32
    b : uint32
    c : uint32
    d : uint32
  }

(*
md5round :: Array Int Word32 -> MD5 -> Int -> MD5
md5round datA (MD5 a b c d) i =
    let f  =  funA ! i
        w  =  datA ! (idxA ! i)
        a' =  b + (a + f b c d + w + sinA ! i) `rotateL` rotA ! i
    in MD5 d a' b c
*)
let md5round (datA:uint32[]) {MD5.a=a; MD5.b=b; MD5.c=c; MD5.d=d} i =
  let f = funA.[i]
  let w = datA.[idxA.[i]]
  let a' = (b + (a + (f b c d) + w + sinA.[i])) <<< rotA.[i]
  {a=d; b=a'; c=b; d=c}

(*
(<+>) :: MD5 -> BL.ByteString -> MD5
infixl 5  <+>
md5@(MD5 a b c d) <+> bs =
    let datA = listArray (0,15) $ replicateM 16 getWord32le `runGet` bs
        MD5 a' b' c' d' = foldl' (md5round datA) md5 [1..64]
    in MD5 (a + a') (b + b') (c + c') (d + d')
*)
let md5plus md5 (bs:byte[]) =
  let a, b, c, d = md5.a, md5.b, md5.c, md5.d 
  let datA =
    bs 
    |> Array.chunkBySize 4
    |> Array.take 16
    |> Array.map
      (fun elts ->
        match elts with
        | [| var1; var2; var3; var4 |] -> 
          0ul ||| (uint32 var1 <<< 24) ||| (uint32 var2 <<< 16) 
            ||| (uint32 var3 <<< 8) ||| uint32 var4
        | _ -> failwith "md5plus.datA"
      )
  let md5' = List.fold (md5round datA) md5 [0..63]
  {a=a+md5'.a; b=b+md5'.b; c=c+md5'.c; d=d+md5'.d}

let padMessage (msgString:string) =
  let msg = System.Text.Encoding.ASCII.GetBytes msgString
  let msgLen = Array.length msg

  // no of padding bits for 448 mod 512
  let pad = 
    let calc = ((448 - ((8 * msgLen) % 512)) + 512) % 512
    if calc = 0 then 512u else uint32 calc
  
  // buffer size in multiple of bytes
  let sizeMsgBuf = (uint32 msgLen) + (pad/8u) + 8u

  // buffer size in multiple of bytes
  let sizeMsg = msgLen * 8 |> uint64

  // buffer to hold bits
  let bMsg = Array.create 64 0uy

  // Copy string to buffer
  for i = 0 to msgLen - 1 do 
    Array.set bMsg i msg.[i]

  // making first bit of padding 1
  Array.set bMsg msgLen (bMsg.[msgLen] ||| 0x80uy)

  // write the size value
  for i = 8 downto 1 do
    Array.set bMsg ((int sizeMsgBuf)-i) 
      (((sizeMsg >>> ((8-i)*8)) &&& 0x00000000000000ffUL) |> byte)

  System.Text.Encoding.ASCII.GetString bMsg


let md5sum (msg: string) =

  msg
