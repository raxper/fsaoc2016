(* FizzBuzz *)

let main input = 
  let f x = if x % 3 = 0 then "Fizz" else ""
  let b x = if x % 5 = 0 then "Buzz" else ""
  let ans = (f input) + (b input)
  if ans = "" then input.ToString() else ans
