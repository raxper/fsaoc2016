(*
#load @"c:\Users\SVShah\Projects\fsaoc2016\2\TwoMain.fsx";; open TwoMain;;
*)

open System.Text.RegularExpressions
open System.IO

(* The initial button for each code *)
//let startButton = '5'

(* The Directions that you can move (no diagonals allowed) *)
type Direction = | L | R | D | U 

(* Converts a string to a Direction *)
let toDirection = function
  | "L" -> L
  | "R" -> R
  | "D" -> D
  | "U" -> U
  | _ -> failwith "toDirection"

(* Performs a single move (given an input, provides an output) *)
let move curBtn input =
  match input with
  | L -> 
    match curBtn with
    | '1' | '2' -> '1'
    | '3' -> '2'
    | '4' | '5' -> '4'
    | '6' -> '5'
    | '7' | '8' -> '7'
    | '9' -> '8'
    | _ -> failwith "move.L"
  | R -> 
    match curBtn with
    | '1' -> '2'
    | '2' | '3' -> '3'
    | '4' -> '5'
    | '5' | '6' -> '6'
    | '7' -> '8'
    | '8' | '9' -> '9'
    | _ -> failwith "move.R"
  | D ->
    match curBtn with
    | '1' -> '4'
    | '2' -> '5'
    | '3' -> '6'
    | '4' | '7' -> '7'
    | '5' | '8' -> '8'
    | '6' | '9' -> '9'
    | _ -> failwith "move.D"
  | U ->
    match curBtn with
    | '1' | '4' -> '1'
    | '2' | '5' -> '2'
    | '3' | '6' -> '3'
    | '7' -> '4'
    | '8' -> '5'
    | '9' -> '6'
    | _ -> failwith "move.U"

(* 
  For a single character of the code, performs all moves required to get the
  final value.
*)
let moveAll mFunc stBtn mInstr =
  Regex.Matches (mInstr, @".")
  |> Seq.cast
  |> Seq.map 
    (fun (x : System.Text.RegularExpressions.Match) -> 
      x.Value |> toDirection)
  |> Seq.fold mFunc stBtn

(* Puts together all the values to get a complete, final code *)
let getCode instr func stBtn =
  Regex.Matches (instr, @"[ULRD]+")
  |> Seq.cast
  |> Seq.map 
    (fun (x : System.Text.RegularExpressions.Match) -> 
      (x.Value |> moveAll func stBtn).ToString())
  |> Seq.fold (+) "" 

(* Holds the instructions *)
let sw = new StringWriter()
fprintfn sw 
  "LURLDDLDULRURDUDLRULRDLLRURDUDRLLRLRURDRULDLRLRRDDULUDULURULLURLURRRLLDURURLLUURDLLDUUDRRDLDLLRUUDURURRULURUURLDLLLUDDUUDRULLRUDURRLRLLDRRUDULLDUUUDLDLRLLRLULDLRLUDLRRULDDDURLUULRDLRULRDURDURUUUDDRRDRRUDULDUUULLLLURRDDUULDRDRLULRRRUUDUURDULDDRLDRDLLDDLRDLDULUDDLULUDRLULRRRRUUUDULULDLUDUUUUDURLUDRDLLDDRULUURDRRRDRLDLLURLULDULRUDRDDUDDLRLRRDUDDRULRULULRDDDDRDLLLRURDDDDRDRUDUDUUDRUDLDULRUULLRRLURRRRUUDRDLDUDDLUDRRURLRDDLUUDUDUUDRLUURURRURDRRRURULUUDUUDURUUURDDDURUDLRLLULRULRDURLLDDULLDULULDDDRUDDDUUDDUDDRRRURRUURRRRURUDRRDLRDUUULLRRRUDD"
fprintfn sw
  "DLDUDULDLRDLUDDLLRLUUULLDURRUDLLDUDDRDRLRDDUUUURDULDULLRDRURDLULRUURRDLULUDRURDULLDRURUULLDLLUDRLUDRUDRURURUULRDLLDDDLRUDUDLUDURLDDLRRUUURDDDRLUDDDUDDLDUDDUUUUUULLRDRRUDRUDDDLLLDRDUULRLDURLLDURUDDLLURDDLULLDDDRLUDRDDLDLDLRLURRDURRRUDRRDUUDDRLLUDLDRLRDUDLDLRDRUDUUULULUDRRULUDRDRRLLDDRDDDLULURUURULLRRRRRDDRDDRRRDLRDURURRRDDULLUULRULURURDRRUDURDDUURDUURUURUULURUUDULURRDLRRUUDRLLDLDRRRULDRLLRLDUDULRRLDUDDUUURDUDLDDDUDL"
fprintfn sw
  "RURDRUDUUUUULLLUULDULLLDRUULURLDULULRDDLRLLRURULLLLLLRULLURRDLULLUULRRDURRURLUDLULDLRRULRDLDULLDDRRDLLRURRDULULDRRDDULDURRRUUURUDDURULUUDURUULUDLUURRLDLRDDUUUUURULDRDUDDULULRDRUUURRRDRLURRLUUULRUDRRLUDRDLDUDDRDRRUULLLLDUUUULDULRRRLLRLRLRULDLRURRLRLDLRRDRDRLDRUDDDUUDRLLUUURLRLULURLDRRULRULUDRUUURRUDLDDRRDDURUUULLDDLLDDRUDDDUULUDRDDLULDDDDRULDDDDUUUURRLDUURULRDDRDLLLRRDDURUDRRLDUDULRULDDLDDLDUUUULDLLULUUDDULUUDLRDRUDLURDULUDDRDRDRDDURDLURLULRUURDUDULDDLDDRUULLRDRLRRUURRDDRDUDDLRRLLDRDLUUDRRDDDUUUDLRRLDDDUDRURRDDUULUDLLLRUDDRULRLLLRDLUDUUUUURLRRUDUDDDDLRLLULLUDRDURDDULULRDRDLUDDRLURRLRRULRL"
fprintfn sw 
  "LDUURLLULRUURRDLDRUULRDRDDDRULDLURDDRURULLRUURRLRRLDRURRDRLUDRUUUULLDRLURDRLRUDDRDDDUURRDRRURULLLDRDRDLDUURLDRUULLDRDDRRDRDUUDLURUDDLLUUDDULDDULRDDUUDDDLRLLLULLDLUDRRLDUUDRUUDUDUURULDRRLRRDLRLURDRURURRDURDURRUDLRURURUUDURURUDRURULLLLLUDRUDUDULRLLLRDRLLRLRLRRDULRUUULURLRRLDRRRDRULRUDUURRRRULDDLRULDRRRDLDRLUDLLUDDRURLURURRLRUDLRLLRDLLDRDDLDUDRDLDDRULDDULUDDLLDURDULLDURRURRULLDRLUURURLLUDDRLRRUUDULRRLLRUDRDUURLDDLLURRDLRUURLLDRDLRUULUDURRDULUULDDLUUUDDLRRDRDUDLRUULDDDLDDRUDDD"
fprintfn sw
  "DRRDRRURURUDDDRULRUDLDLDULRLDURURUUURURLURURDDDDRULUDLDDRDDUDULRUUULRDUDULURLRULRDDLDUDLDLULRULDRRLUDLLLLURUDUDLLDLDRLRUUULRDDLUURDRRDLUDUDRULRRDDRRLDUDLLDLURLRDLRUUDLDULURDDUUDDLRDLUURLDLRLRDLLRUDRDUURDDLDDLURRDDRDRURULURRLRLDURLRRUUUDDUUDRDRULRDLURLDDDRURUDRULDURUUUUDULURUDDDDUURULULDRURRDRDURUUURURLLDRDLDLRDDULDRLLDUDUDDLRLLRLRUUDLUDDULRLDLLRLUUDLLLUUDULRDULDLRRLDDDDUDDRRRDDRDDUDRLLLDLLDLLRDLDRDLUDRRRLDDRLUDLRLDRUURUDURDLRDDULRLDUUUDRLLDRLDLLDLDRRRLLULLUDDDLRUDULDDDLDRRLLRDDLDUULRDLRRLRLLRUUULLRDUDLRURRRUULLULLLRRURLRDULLLRLDUUUDDRLRLUURRLUUUDURLRDURRDUDDUDDRDDRUD"

let day2part1 = getCode (sw.ToString()) move '5'

(* Part 2 *)

(* New keypad *)
let move2 curBtn input =
  match input with
  | L -> 
    match curBtn with
    | '1' -> '1' 
    | '2' | '3' -> '2'
    | '4' -> '3'
    | '5' | '6' -> '5'
    | '7' -> '6'
    | '8' -> '7'
    | '9' -> '8'
    | 'A' | 'B' -> 'A'
    | 'C' -> 'B'
    | 'D' -> 'D'
    | _ -> failwith "move2.L"
  | R -> 
    match curBtn with
    | '1' -> '1'
    | '2' -> '3'
    | '3' | '4' -> '4'
    | '5' -> '6'
    | '6' -> '7'
    | '7' -> '8'
    | '8' | '9' -> '9'
    | 'A' -> 'B'
    | 'B' | 'C' -> 'C'
    | 'D' -> 'D'
    | _ -> failwith "move2.R"
  | D ->
    match curBtn with
    | '1' -> '3'
    | '2' -> '6'
    | '3' -> '7'
    | '4' -> '8'
    | '5' -> '5'
    | '6' | 'A' -> 'A'
    | '7' -> 'B'
    | '8' | 'C' -> 'C'
    | '9' -> '9'
    | 'B' | 'D' -> 'D'
    | _ -> failwith "move2.D"
  | U ->
    match curBtn with
    | '1' | '3' -> '1'
    | '2' | '6' -> '2'
    | '4' | '8' -> '4'
    | '5' -> '5'
    | '7' -> '3'
    | '9' -> '9'
    | 'A' -> '6'
    | 'B' -> '7'
    | 'C' -> '8'
    | 'D' -> 'B'
    | _ -> failwith "move2.U"

(* 
  Makes it so that the last character of the previous digit is used as the 
  first character of the new digit.  The first digit gets a start value of
  '5'
*)
let getCode2 instr func stBtn =
  Regex.Matches (instr, @"[ULRD]+")
  |> Seq.cast
  |> Seq.map (fun (x : System.Text.RegularExpressions.Match) -> x.Value)
  |> Seq.fold 
    (fun (ans, st) nxtInstr -> 
      let nxtChar = moveAll func st nxtInstr in
      (ans + nxtChar.ToString(), nxtChar)
    ) 
    ("", stBtn)

let day2part2 = getCode2 (sw.ToString()) move2 '5' |> fst
Printf.printfn "day2part2: %s" day2part2

let day2part2ctrl = getCode (sw.ToString()) move2 '5' 
Printf.printfn "day2part2ctrl: %s" day2part2ctrl