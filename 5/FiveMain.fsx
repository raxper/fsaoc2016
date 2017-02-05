(*
#load @"c:\Users\SVShah\Projects\fsaoc2016\5\FiveTest.fsx";; open FiveMain;; open FiveTest;;
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

/// MD5 algorithm
/// https://en.wikipedia.org/wiki/MD5#Algorithm 
/// https://rosettacode.org/wiki/MD5/Implementation#Java 
let md5 (msg: string) =
  // Setup constants
  /// specifies the per-round shift amounts
  let md5Shift : uint32 list = 
    [7; 12; 17; 22; 
    5; 9; 14; 20;
    4; 11; 16; 23;
    6; 10; 15; 21]
    |> List.map uint32
  
  /// Use binary integer part of the sines of integers (Radians) as constants
  /// TABLE_T[i] = floor(2^32 x abs(sin(i + 1)))
  let tableT : uint32 list = 
    [0xd76aa478; 0xe8c7b756; 0x242070db; 0xc1bdceee; 0xf57c0faf; 0x4787c62a; 0xa8304613; 0xfd469501; 0x698098d8; 0x8b44f7af; 0xffff5bb1; 0x895cd7be; 0x6b901122; 0xfd987193; 0xa679438e; 0x49b40821; 0xf61e2562; 0xc040b340; 0x265e5a51; 0xe9b6c7aa; 0xd62f105d; 0x02441453; 0xd8a1e681; 0xe7d3fbc8; 0x21e1cde6; 0xc33707d6; 0xf4d50d87; 0x455a14ed; 0xa9e3e905; 0xfcefa3f8; 0x676f02d9; 0x8d2a4c8a; 0xfffa3942; 0x8771f681; 0x6d9d6122; 0xfde5380c; 0xa4beea44; 0x4bdecfa9; 0xf6bb4b60; 0xbebfbc70; 0x289b7ec6; 0xeaa127fa; 0xd4ef3085; 0x04881d05; 0xd9d4d039; 0xe6db99e5; 0x1fa27cf8; 0xc4ac5665; 0xf4292244; 0x432aff97; 0xab9423a7; 0xfc93a039; 0x655b59c3; 0x8f0ccc92; 0xffeff47d; 0x85845dd1; 0x6fa87e4f; 0xfe2ce6e0; 0xa3014314; 0x4e0811a1; 0xf7537e82; 0xbd3af235; 0x2ad7d2bb; 0xeb86d391]
    |> List.map uint32
  
  let a0 = 0x67452301ul // A
  let b0 = 0xefcdab89ul // B 
  let c0 = 0x98badcfeul // C 
  let d0 = 0x10325476ul // D 

  (****************************************************************************)
  // Algorithm 
  let msgBytes = System.Text.Encoding.ASCII.GetBytes msg
  let messageLenBytes = Array.length msgBytes
  let numBlocks = (((messageLenBytes + 8 |> uint32) >>> 6) |> int) + 1
  let totalLen = numBlocks <<< 6
  
  // append "0" bit until message length in bits ≡ 448 (mod 512)
  // append "1" bit to message
  let paddingBytes = Array.create (totalLen - messageLenBytes) 0uy
  Array.set paddingBytes 0 0x80uy

  // append original length in bits mod (2 pow 64) to message
  let messageLenBits = (uint64 messageLenBytes) <<< 3
  for i = 0 to 7 do
    Array.set paddingBytes (Array.length paddingBytes - 8 + i) 
      (messageLenBits >>> (8*i) |> byte)
  
  // to hold 'break chunk into sixteen 32-bit words M[j], 0 ≤ j ≤ 15'
  let buffer = Array.create 16 0

  let mutable a = a0
  let mutable b = b0
  let mutable c = c0
  let mutable d = d0

  for i = 0 to numBlocks-1 do
    
    ()

  msg

// TODO: Try to mimic haskell implementation on rosettacode