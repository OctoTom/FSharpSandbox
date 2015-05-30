// Friday fun exercise
// http://stackoverflow.com/questions/30506976/f-tail-recursion-and-continuation-with-lists

// Original version
let rec f i list = 
  match list with
  | [] -> [i]
  | x::xs -> i+x :: f (i+1) xs

// Tail recursive version with accumulator
let fA i list =
  let rec loop i acc list =
    match list with
    | x::xs ->
      let newI = i + 1
      let newAcc = x + i :: acc
      let newList = xs
      loop newI newAcc newList
    | [] -> List.rev (i::acc)
  loop i [] list

// Continuation based version
let fC i list =
  let rec loop i (cont:int list -> int list) list =
    match list with 
    | x::xs ->
      let newI = i + 1
      let newCont = fun res -> cont (x + i :: res)
      let newList = xs
      loop newI newCont newList
    | [] -> cont (i::list) 
  loop i id list

// All these expressions evaluate to [10; 12; 14; 16; 14]
let res1 = f 10 [0..3]
let res2 = fA 10 [0..3]
let res3 = fC 10 [0..3]
//-------------------------------------------------------