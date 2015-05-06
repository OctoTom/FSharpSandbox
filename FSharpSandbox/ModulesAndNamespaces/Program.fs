module Program

let res = Module2.myFunc 3
 
[<EntryPoint>]
let main argv = 
  printfn "%A" (Module2.myFunc 5)
  0 // return an integer exit code
