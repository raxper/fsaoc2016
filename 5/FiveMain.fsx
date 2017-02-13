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
let funG x y z : uint32 = (z &&& x) ||| (~~~z &&& y)
let funH x y z : uint32 = x ^^^ y ^^^ z
let funI x y z : uint32 = y ^^^ (x ||| ~~~z)

(* 
idxF, idxG, idxH, idxI :: Int -> Int
idxF i = i
idxG i = (5 * i + 1) `mod` 16
idxH i = (3 * i + 5) `mod` 16
idxI i = 7 * i `mod` 16
*)
let idxF i : int = i
let idxG i = (5*i + 1) % 16
let idxH i = (3*i + 5) % 16
let idxI i = (7*i) % 16

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

/// specifies the per-round shift amounts
let rotA : int list =
  [[7; 12; 17; 22]; [5; 9; 14; 20]; [4; 11; 16; 23]; [6; 10; 15; 21]]
  |> List.collect (List.replicate 4)
  |> List.concat

/// for i in 0 to 63 do floor(2^32 × abs(sin(i + 1)))
/// Use binary integer part of the sines of integers (Radians) as constants
let sinA =
  [1. .. 64.] |> List.map (sin >> abs >> (( * ) (2.**32.)) >> floor >> uint32)

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

let initialMD5 =
  {
    a = 0x67452301u
    b = 0xefcdab89u
    c = 0x98badcfeu
    d = 0x10325476u
  }

(*
md5round :: Array Int Word32 -> MD5 -> Int -> MD5
md5round datA (MD5 a b c d) i =
    let f  =  funA ! i
        w  =  datA ! (idxA ! i)
        a' =  b + (a + f b c d + w + sinA ! i) `rotateL` rotA ! i
    in MD5 d a' b c
*)
/// This is the "Main loop" in "Process the message in successive 512-bit chunks" that goes from 0 to 63. The process starts with the initial MD5 for each run of the loop from 0 to 63.
let md5round (datA:uint32[]) {MD5.a=a; MD5.b=b; MD5.c=c; MD5.d=d} i =
  let rotateL r x = (x<<<r) ||| (x>>>(32-r))
  let f = funA.[i]
  let w = datA.[idxA.[i]]
  let a' = b + (a + (f b c d) + sinA.[i] + w |> rotateL rotA.[i])
  {a=d; b=a'; c=b; d=c}

(*
(<+>) :: MD5 -> BL.ByteString -> MD5
infixl 5  <+>
md5@(MD5 a b c d) <+> bs =
    let datA = listArray (0,15) $ replicateM 16 getWord32le `runGet` bs
        MD5 a' b' c' d' = foldl' (md5round datA) md5 [1..64]
    in MD5 (a + a') (b + b') (c + c') (d + d')
*)
/// This is the code that does 1 run-through (i.e. 1 512-bit chunk) of the "Process the message in successive 512-bit chunks" loop.
let md5plus m (bs:byte[]) =
  let datA =
    bs
    |> Array.chunkBySize 4
    |> Array.take 16
    |> Array.map (fun elt -> System.BitConverter.ToUInt32(elt, 0))
  let m' = List.fold (md5round datA) m [0..63]
  {a=m.a+m'.a; b=m.b+m'.b; c=m.c+m'.c; d=m.d+m'.d}
  
let padMessage (msg:byte[]) =
  let msgLen = Array.length msg
  let msgLenInBits = (uint64 msgLen) * 8UL
  let lastSegmentSize = let m = msgLen % 64 in if m = 0 then 64 else m
  let pad =
    Array.create 
      (if lastSegmentSize >= 56 then (64 - lastSegmentSize + 64)
      else 64 - lastSegmentSize)
      0uy
  Array.set pad 0 0x80uy 
  for i = 0 to 7 do
    Array.set 
      pad
      (Array.length pad - 8 + i)
      ((msgLenInBits >>> (8*i)) |> byte)
  Array.append msg pad 

let md5sum (msg: string) =
  System.Text.Encoding.ASCII.GetBytes msg
  |> padMessage
  |> Array.chunkBySize 64
  |> Array.fold md5plus initialMD5
  |> (fun {MD5.a=a; MD5.b=b; MD5.c=c; MD5.d=d} -> 
    System.BitConverter.GetBytes a
    |> (fun x -> Array.append x <| System.BitConverter.GetBytes b)
    |> (fun x -> Array.append x <| System.BitConverter.GetBytes c)
    |> (fun x -> Array.append x <| System.BitConverter.GetBytes d))
  |> Array.map (sprintf "%02X")
  |> Array.reduce ( + )

/// Hard-coded input from problem
let day5part1input = "cxdnnyjw"
/// Criteria for day 5, part 1 from problem
let day5part1criteria = "00000"

/// Day 5 Part 1
let day5part1 input crit =
  Seq.initInfinite (fun i -> input + string i)
  |> Seq.map md5sum
  |> Seq.filter (fun m -> m.StartsWith crit)
  |> Seq.take 8
  |> Seq.map (Seq.item 5 >> string)
  |> Seq.reduce ( + )

// Takes a while to run 
// printfn "Day 5 Part 1: %s" (day5part1 day5part1input day5part1criteria)

#r @"C:\Users\SVShah\Projects\fsaoc2016\packages\FSharp.Collections.ParallelSeq\lib\net40\FSharp.Collections.ParallelSeq.dll"
open FSharp.Collections.ParallelSeq
open System.Collections.Generic

/// Day 5 Part 2 - this is wasteful but after spending a week on MD5, I don't care
let day5part2 input =
  let answer = Dictionary<char, char>(8)
  let add (d:Dictionary<_,_>) kv = 
    if not <| d.ContainsKey(fst kv) then 
      d.Add(fst kv, snd kv)
    else ()
  let notComplete (d:Dictionary<_,_>) = d.Count < 8

  Seq.initInfinite (fun i -> input + string i)
  |> Seq.map md5sum
  |> Seq.filter
    (fun m -> 
      m.[0] = '0' && m.[1] = '0' && m.[2] = '0' && m.[3] = '0' && m.[4] = '0'
        && (m.[5] = '0' || m.[5] = '1' || m.[5] = '2' || m.[5] = '3' 
        || m.[5] = '4' || m.[5] = '5' || m.[5] = '6' || m.[5] = '7'))
  |> Seq.map (fun m -> m.[5], m.[6])
  |> Seq.takeWhile (fun _ -> notComplete answer)
  |> Seq.iter (fun m -> add answer m)

  (answer.['0'] |> string) + (answer.['1'] |> string) + (answer.['2'] |> string)
    + (answer.['3'] |> string) + (answer.['4'] |> string) 
    + (answer.['5'] |> string) + (answer.['6'] |> string) 
    + (answer.['7'] |> string)
// C9E29889 is wrong
// 999828EC is right