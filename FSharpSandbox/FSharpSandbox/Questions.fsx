// Container
type M<'a> = M of 'a * string list
  
// Creates a loggable version of given function
let lift1 s f x = M(f x, [s])
let lift2 s f x y = M(f x y, [s])

let succ a = a + 1
let succM = lift1 "Applying succM()." succ
  
let add a b = a + b
let addM = lift2 "Applying addM()." add


let f x = x + 1
let g x y = x + y

let lift1 s f x = (f x, s)
let lift2 s f x y = (f x y, s)

let fLifted = lift1 "Func f" f
let gLifted = lift2 "Func g" g