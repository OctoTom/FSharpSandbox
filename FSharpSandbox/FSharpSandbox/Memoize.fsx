let add (a, b) =
  printfn "Adding %A and %A" a b
  a + b

let sub (a, b) =
  printfn "Subtracting %A and %A" a b
  a - b

let memoize (f:'a->'b) =
  let dict = new System.Collections.Generic.Dictionary<'a, 'b>()
  let memoizedFunction x =
    if dict.ContainsKey(x)
      then
        printfn "Using memoized value for %A" x
        dict.[x]
      else
        printfn "The value %A not found in dictionary. Have to compute it." x
        let y = f x
        dict.Add(x, y)
        y
  memoizedFunction
      
let memAdd = memoize add

let res = memAdd (1, 4)



