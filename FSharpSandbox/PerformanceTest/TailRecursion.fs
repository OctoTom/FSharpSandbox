// This test demonstrates the differences in call stack of recursive and tai-recursive sum.
module TailRecursion

// Normal sum
let rec sumList a =
  match a with
  | [] -> 0 // Put breakpoint here and check out call stack.
  | h::t -> h + sumList t

// Tail recursive sum
let sumListTailRecursive list =
  let rec sumListAccu state list = // Accumulator
    match list with
    | [] -> state // Put breakpoint here and check out call stack.
    | h::t -> sumListAccu (state + h) t
  sumListAccu 0 list

// Test
let Test_TailRecursion =
  let res1 = sumList [0..100]
  let res2 = sumListTailRecursive [0..100]
  printfn "%A" res1
  printfn "%A" res2


