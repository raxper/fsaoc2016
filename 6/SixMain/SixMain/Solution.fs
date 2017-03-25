module Solution

open System.IO

/// Generic tee function for debugging purposes.
let tee f x = f x |> ignore; x 

/// Gives the most frequent character in a particular position from all strings. If all the strings in the list are smaller in length than the specified position, this function will throw a System.ArgumentException. 
/// pos is zero-based.
let mostFreqentCharAt pos list =
    list
    |> Seq.filter (fun line -> String.length line >= (pos+1)) 
    |> Seq.map (Seq.item pos)
    |> Seq.countBy id
    |> Seq.sortByDescending snd
    |> Seq.head
    |> fst

/// Gives the length of the longest string in the list.
///
/// Intended to help with the invocation of mostFreqentCharAt
let longestLength list =
    list
    |> Seq.map (String.length)
    |> Seq.max

/// Day 6 Part 1
let day6part1 filename =
    let lines = File.ReadAllLines filename
    seq {
      for i = 0 to (longestLength lines - 1) do
        yield (mostFreqentCharAt i lines |> string)
    }
    |> Seq.reduce ( + )

/// Gives the least frequent character in a particular position from all strings. If all the strings in the list are smaller in length than the specified position, this function will throw a System.ArgumentException. 
/// pos is zero-based.
let leastFreqentCharAt pos list =
    list
    |> Seq.filter (fun line -> String.length line >= (pos+1)) 
    |> Seq.map (Seq.item pos)
    |> Seq.countBy id
    |> Seq.sortBy snd
    |> Seq.head
    |> fst

/// Day 6 Part 2
let day6part2 filename =
    let lines = File.ReadAllLines filename
    seq {
      for i = 0 to (longestLength lines - 1) do
        yield (leastFreqentCharAt i lines |> string)
    }
    |> Seq.reduce ( + )
