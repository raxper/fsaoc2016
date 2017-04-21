(*
#load @"./FiveTest.fsx";;
#load @"./FiveMain.fsx";;
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
F(X,Y,Z) = XY v not(X) Z
G(X,Y,Z) = XZ v Y not(Z)
H(X,Y,Z) = X xor Y xor Z
I(X,Y,Z) = Y xor (X v not(Z))
*)
let fxyz x y z : uint32 = (x &&& y) ||| (~~~x &&& z)
let gxyz x y z : uint32 = (z &&& x) ||| (~~~z &&& y)
let hxyz x y z : uint32 = x ^^^ y ^^^ z
let ixyz x y z : uint32 = y ^^^ (x ||| ~~~z)

(*
g := i
g := (5×i + 1) mod 16
g := (3×i + 5) mod 16
g := (7×i) mod 16
*)
let g1Idx = id
let g2Idx i = (5*i + 1) % 16
let g3Idx i = (3*i + 5) % 16
let g4Idx i = (7*i) % 16

// /////////////////////////////////////////////////////////////////////////////
// Define Arrays
// /////////////////////////////////////////////////////////////////////////////

(*
funA = listArray (1,64) $ replicate 16 =<< [funF, funG, funH, funI]
*)
let fghi =
  [fxyz; gxyz; hxyz; ixyz]
  |> List.collect (List.replicate 16)

(*
g := i
g := (5×i + 1) mod 16
g := (3×i + 5) mod 16
g := (7×i) mod 16

idxA = listArray (1,64) $ zipWith ($) (replicate 16 =<< [idxF, idxG, idxH, idxI]) [0..63]
*)
let gIdxs =
  [g1Idx; g2Idx; g3Idx; g4Idx]
  |> List.collect (List.replicate 16)
  |> List.map2 (fun idx func -> func idx) [0..63]

(*
private static final int[] SHIFT_AMTS = {
  7, 12, 17, 22,
  5,  9, 14, 20,
  4, 11, 16, 23,
  6, 10, 15, 21
};

static short rot0[] = { 7,12,17,22};
static short rot1[] = { 5, 9,14,20};
static short rot2[] = { 4,11,16,23};
static short rot3[] = { 6,10,15,21};
static short *rots[] = {rot0, rot1, rot2, rot3 };

rotA = listArray (1,64) $ concat . replicate 4 =<<
       [[7, 12, 17, 22], [5, 9, 14, 20], [4, 11, 16, 23], [6, 10, 15, 21]]
*)
/// specifies the per-round shift amounts
let s =
  [[7; 12; 17; 22]; [5; 9; 14; 20]; [4; 11; 16; 23]; [6; 10; 15; 21]]
  |> List.collect (List.replicate 4)
  |> List.concat

(*
for i from 0 to 63
    K[i] := floor(2^32 × abs(sin(i + 1)))
end for
*)
/// Use binary integer part of the sines of integers (Radians) as constants
let k =
  [1. .. 64.] |> List.map (sin >> abs >> (( * ) (2.**32.)) >> floor >> uint32)

// /////////////////////////////////////////////////////////////////////////////
// Define Logic
// /////////////////////////////////////////////////////////////////////////////

(*
The MD5 algorithm assumes a little endian platform, but Windows is big endian.

word A: 01 23 45 67
word B: 89 ab cd ef
word C: fe dc ba 98
word D: 76 54 32 10
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
if 0 ≤ i ≤ 15 then
  F := (B and C) or ((not B) and D)
  g := i
else if 16 ≤ i ≤ 31
  F := (D and B) or ((not D) and C)
  g := (5×i + 1) mod 16
else if 32 ≤ i ≤ 47
  F := B xor C xor D
  g := (3×i + 5) mod 16
else if 48 ≤ i ≤ 63
  F := C xor (B or (not D))
  g := (7×i) mod 16
//Be wary of the below definitions of a,b,c,d
dTemp := D
D := C
C := B
B := B + leftrotate((A + F + K[i] + M[g]), s[i])
A := dTemp

md5round :: Array Int Word32 -> MD5 -> Int -> MD5
md5round datA (MD5 a b c d) i =
    let f  =  funA ! i
        w  =  datA ! (idxA ! i)
        a' =  b + (a + f b c d + w + sinA ! i) `rotateL` rotA ! i
    in MD5 d a' b c
*)
/// This is the "Main loop" in "Process the message in successive 512-bit chunks" that goes from 0 to 63. The process starts with the initial MD5 for each run of the loop from 0 to 63.
let md5round (msg:uint32[]) {MD5.a=a; MD5.b=b; MD5.c=c; MD5.d=d} i =
  let rotateL32 r x = (x<<<r) ||| (x>>>(32-r))
  let f = fghi.[i] b c d
  let a' = b + (a + f + k.[i] + msg.[gIdxs.[i]] |> rotateL32 s.[i])
  {a=d; b=a'; c=b; d=c}

(*
//Process the message in successive 512-bit chunks:
for each 512-bit chunk of message
    break chunk into sixteen 32-bit words M[j], 0 ≤ j ≤ 15
//Initialize hash value for this chunk:
    var int A := a0
    var int B := b0
    var int C := c0
    var int D := d0
//Main loop (md5round):
    for i from 0 to 63
        ...
    end for
//Add this chunk's hash to result so far:
    a0 := a0 + A
    b0 := b0 + B
    c0 := c0 + C
    d0 := d0 + D
end for

(<+>) :: MD5 -> BL.ByteString -> MD5
infixl 5  <+>
md5@(MD5 a b c d) <+> bs =
    let datA = listArray (0,15) $ replicateM 16 getWord32le `runGet` bs
        MD5 a' b' c' d' = foldl' (md5round datA) md5 [1..64]
    in MD5 (a + a') (b + b') (c + c') (d + d')
*)
/// This is the code that does 1 run-through (i.e. 1 512-bit / 64-byte chunk) of the "Process the message in successive 512-bit chunks" loop.
let md5plus m (bs:byte[]) =
  let datA =
    bs
    |> Array.chunkBySize 4
    |> Array.take 16
    |> Array.map (fun elt -> System.BitConverter.ToUInt32(elt, 0))
  let m' = List.fold (md5round datA) m [0..63]
  {a=m.a+m'.a; b=m.b+m'.b; c=m.c+m'.c; d=m.d+m'.d}

(*
int messageLenBytes = message.length;
int numBlocks = ((messageLenBytes + 8) >>> 6) + 1;
int totalLen = numBlocks << 6;
byte[] paddingBytes = new byte[totalLen - messageLenBytes];
paddingBytes[0] = (byte)0x80;

long messageLenBits = (long)messageLenBytes << 3;
for (int i = 0; i < 8; i++)
{
  paddingBytes[paddingBytes.length - 8 + i] = (byte)messageLenBits;
  messageLenBits >>>= 8;
}
*)
/// Pads a message so that length is a multiple of 64 bytes AND there's enough space to put the size at the end (8 bytes)
let padMessage(msg : byte []) =
  let msgLen = Array.length msg
  let msgLenInBits = (uint64 msgLen) * 8UL
  let lastSegmentSize =
    let m = msgLen % 64
    if m = 0 then 64
    else m
  let pad =
    Array.create (if lastSegmentSize >= 56 then (64 - lastSegmentSize + 64)
                  else 64 - lastSegmentSize) 0uy
  Array.set pad 0 0x80uy
  for i = 0 to 7 do
    Array.set pad (Array.length pad - 8 + i)
      ((msgLenInBits >>> (8 * i)) |> byte)
  Array.append msg pad

let md5sum (msg: string) =
  System.Text.Encoding.ASCII.GetBytes msg
  |> padMessage
  |> Array.chunkBySize 64
  |> Array.fold md5plus initialMD5
  |> (fun {MD5.a=a; MD5.b=b; MD5.c=c; MD5.d=d} ->
    System.BitConverter.GetBytes a
    |> (fun x -> System.BitConverter.GetBytes b |> Array.append x)
    |> (fun x -> System.BitConverter.GetBytes c |> Array.append x)
    |> (fun x -> System.BitConverter.GetBytes d |> Array.append x))
  |> Array.map (sprintf "%02X")
  |> Array.reduce ( + )

/// Hard-coded input from problem
let day5part1input = "cxdnnyjw"
/// Criteria for day 5, part 1 from problem
let day5part1criteria = "00000"

/// Day 5 Part 1
let day5part1 input crit (md5sum:string -> string) =
  Seq.initInfinite (fun i -> input + string i)
  |> Seq.map md5sum
  |> Seq.filter (fun m -> m.StartsWith crit)
  |> Seq.take 8
  |> Seq.map (Seq.item 5 >> string)
  |> Seq.reduce ( + )

// Takes a while to run
// printfn "Day 5 Part 1: %s" (day5part1 day5part1input day5part1criteria md5sum)

open System.Collections.Generic

/// Day 5 Part 2
let day5part2 input crit (md5sum: string -> string) =
  let answer = Dictionary<int, string> 8
  let add (d:Dictionary<_,_>) kv =
    if not <| d.ContainsKey(fst kv) then
      d.Add(fst kv, snd kv)
    else ()
  let notComplete (d:Dictionary<_,_>) = d.Count < 8

  Seq.initInfinite (fun i -> input + string i)
  |> Seq.map md5sum
  |> Seq.filter
    (fun m ->
      (m.StartsWith crit)
        && (m.[5] = '0' || m.[5] = '1' || m.[5] = '2' || m.[5] = '3'
        || m.[5] = '4' || m.[5] = '5' || m.[5] = '6' || m.[5] = '7'))
  |> Seq.map (fun m -> m.[5] |> string |> System.Int32.Parse, m.[6] |> string)
  |> Seq.takeWhile (fun _ -> notComplete answer)
  |> Seq.iter (fun m -> add answer m)

  [
    for i in 0 .. 7 do
      yield answer.[i]
  ]
  |> List.reduce ( + )

let md5ootb (msg: string) =
  use md5 = System.Security.Cryptography.MD5.Create()
  msg
  |> System.Text.Encoding.ASCII.GetBytes
  |> md5.ComputeHash
  |> Seq.map (fun c -> c.ToString("X2"))
  |> Seq.reduce ( + )

(* Takes a long time to run:
#time
day5part2 day5part1input day5part1criteria md5sum
#time
*)
// C9E29889 is wrong - parallel seq
// 999828EC is right