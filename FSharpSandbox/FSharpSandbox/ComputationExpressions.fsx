// ===========================================
// Computation expressions (monads) playground
// ===========================================

module ComputationExpressions = // Option-returning function
  let tryDecr x n = 
    printfn "Conditionally decrementing %A by %A" x n
    if x > n then
      printfn "Condition met."
      Some (x - n)
    else
      printfn "Condition not met."
      None
  // Computation expression builder. (Monadic type is the Option type)
  type VerboseMaybeBuilder() =
    member this.Return x = Some x // Defines what "return" does.
    member this.Bind (p, rest) = // Defines what "let!" does.
      match p with
        | None ->
          printfn "Argument of Bind method matched None."
          None
        | Some a ->
          printfn "Argument of Bind method matched Some a."
          rest a
  // Crate an instance of maybe computation expression
  let maybe = new VerboseMaybeBuilder()
  // Define an function using the instance of maybe computation expression
  let maybeDecr_v1 x =
    maybe {
      let! y = tryDecr x 10
      let! z = tryDecr y 30
      let! t = tryDecr z 50
      return t
    }
  // The same as above without syntactic suger. No magic here, just plane F#.
  let maybeDecr_v2 x =
    maybe.Bind (tryDecr x 10, fun y ->
      maybe.Bind (tryDecr y 30, fun z ->
        maybe.Bind (tryDecr z 50, fun t ->
          maybe.Return t)))  
  // Call the function with various values
  let res1 = maybeDecr_v1 100
  let res2 = maybeDecr_v2 100
  let res3 = maybeDecr_v1 30
  let res4 = maybeDecr_v2 30
  let res5 = maybeDecr_v1 10
  let res6 = maybeDecr_v2 10

  // Monad primer from F Sharp Programing / Computation Expression (WikiBooks)
  // 1. Everyday sequential code
  module Sequential =
    // Definitions
    let readLine() = System.Console.ReadLine()
    let printLine(s) = printfn "%s" s
    // Sequential code. It is allowed in F# but not in pure languages.
    let expr1 =
      do printLine "What is yout name?"
      let name = readLine ()
      do printLine ("Hello, " + name + "!")

  // 2. Sequential code written in functional style. ("If you can understand this much, than you can understand any monad.")
  // This is how we can enforce sequential evaluation in pure functional language.
  module SequentialWithFunctional =
    let readLine(f) = f(System.Console.ReadLine()) // These functions are enhanced by continuation function f. "Do something and call f with the result."
    let printLine(s, f) = f(printfn "%s" s)
    // Chained functions
    let expr = 
      printLine("What is your name?", fun () ->
        readLine(fun name ->
          printLine("Hello, " + name + "!", fun () ->
            ()
          )
        )
      )
    // Is this possible with curried functions instead of function accepting tuple?

  // 3. Define my own Bind function. No special builder type yet.
  module DefineOwnBindMethod =
    // Bind takes an expression and a function (continuation, rest of computations).
    // It evaluates the expression and passed it to the function.
    let bind (x, f) = f x
    // These functions are the same as in the first example. 
    // No continuation added as in the second example. Bind takes care of continuation calling.
    let readLine () = System.Console.ReadLine() 
    let printLine s = printfn "%s" s
    let expr = 
      bind(printLine "What is your name?", fun () ->
        bind(readLine(), fun (s:string) ->
          bind(printLine ("Hello " + s + "!"), fun () ->
            ()
          )
        )
      )

  // 4. Using F# computation expression in De-suggered manner
  // This does not use monadic wrapper type M<'a> yet.
  module FSharpComputationExpression =
    let readLine () = System.Console.ReadLine()
    let printLine s = printfn "%s" s
    type StandardBuilder () = // Definition of monad type
      member this.Delay f = f()
      member this.Bind (x, f) = f x
      member this.Return x = x
    let standard = StandardBuilder ()
    
    // De-suggered version
    let expr4 =
      standard.Delay(fun () ->
        standard.Bind(printLine "What is your name?", fun () ->
          standard.Bind(readLine (), fun (s:string) ->
            standard.Bind(printLine ("Hello " + s + "!"), fun () ->
              standard.Return ()
            )
          )
        )
      )
    // Suggered version
    let expr5 =
      standard {
        printLine("What is your name?")
        let! s = readLine()
        return printLine("Hello " + s + "!")
      }
    // Another suggered expression
    let exp5 =
      standard { // Sugered
        let! a = 2
        let! b = a + 3
        let! c = b + 5
        return a + b + c
      }


  // Another example: Define my own Bind function
  // No computation expression builder class.
  // Bind accepts string. If parsing is successfull the result is sent to continuation. If not, None is returned and no continuation is invoked.
  let addThreeNumbers() =
    let bind(input, f) =
      // NOTE: F# translates .NET functions with "out" parameters to functions returning a tuple.
      // (http://hestia.typepad.com/flatlander/2010/07/f-pattern-matching-for-beginners-part-2-decomposition.html)
      match System.Int32.TryParse(input) with 
      | (true, n) when n >= 0 && n <= 100 -> f n
      | _ -> None
    let readString msg =
      let f = 
        printfn "%s" msg
        System.Console.ReadLine ()
      f
    bind(readString "#1: ", fun x ->
      bind(readString "#2: ", fun y ->
        bind(readString "#3: ", fun z -> Some(x + y + z))))
  let result = addThreeNumbers()

  // The same with computation expression builder class.
  // Monadic type is Option.
  type MaybeBuilder() =
    // member this.Bind(m,f) = Option.bind f m
    // Define Bind explicitly (same as above)
    member this.Bind(m, f) =
      match m with
      | Some x -> f x
      | None -> None
    member this.Return x = Some x
    member this.ReturnFrom x = x
  let maybe = MaybeBuilder()
  let readOptionInt msg =
    let readString msg =
      let f = 
        printfn "%s" msg
        System.Console.ReadLine ()
      f
    let str = readString msg
    match System.Int32.TryParse(str) with 
    | (true, n) when n >= 0 && n <= 100 -> Some n
    | _ -> None
  let res =
    maybe {
      let! x = readOptionInt "#1:"
      let! y = readOptionInt "#2:"
      let! z = readOptionInt "#3:"
      return x + y + z
    }

  let aaa = List.sumBy

  // Can the maybe monad be used during list processing.
  let parseInt s =
    match System.Int32.TryParse(s) with 
      | (true, n) when n >= 0 && n <= 100 -> Some n
      | _ -> None
  let maybeList list =
    if list |> List.exists (Option.isNone)
      then None
      else Some (list |> List.map Option.get)
    
  // parseInt "7"
  let list = ["1"; "2"; "3"]
  let add x y = maybe {
    let! x = x
    let! y = y
    return x + y
  }
  //add (Some 1) (None2)

  let sum = maybe {
    let res = list |> List.su
  }

  // My experiments with monadic style.
  // No monadic type M<'a> yet.
  module MyMonadicExperiments =
    // 1. Sequential
    let exp1 =
      let a = 2
      let b = a + 3
      let c = b + 5
      a + b + c
    // 2. Modnadic
    type VerboseIdentityBuilder() = // Definition of monad
      member this.Delay(f) =
        printfn("Function Delay called.")
        f()
      member this.Bind(x, f) =
        printfn "You just bound %A to %A" x f
        f(x) 
      member this.Return(x) =
        printfn("Function Return called.")
        x
    let verbose = VerboseIdentityBuilder()
    // 2a. De-sugared monad-style expression
    let exp2 =
      verbose.Delay(fun () -> // This line is not needed. (together with the last parenthesis.
        verbose.Bind(2, fun a ->
          verbose.Bind(a + 3, fun b ->
            verbose.Bind(b + 5, fun c ->
              verbose.Return(a + b + c)
            )
          )
        )
      ) 
    // 2.b Sugared monad-style expression
    let exp3 =
      verbose { // Sugared
        let! a = 2
        let! b = a + 3
        let! c = b + 5
        return a + b + c
      }

    // 3. In the same style as above with my own Bind method.
    let bind(x, f) = // my Bind method
      f(x)
    let exp4 =
      bind(2, fun a ->
        bind(a + 3, fun b ->
          bind(b + 5, fun c ->
            a + b + c
          )
        )
      )
// Exercise: Do the same as above with some monadic container type.


          
// LOGGING COMPUTATION EXPRESSION //
module LoggingComputationExpression =
  // Monadic type
  type M<'T> = Log of 'T * string list // Means: 'T * (string list)
  // Computation expression buider. (Definition of the two essential monad function Bind and Return)
  type LoggingBuilder () =
    member x.Bind(monadicInputValue, transformation) =
      let (Log(inputValue, logs)) = monadicInputValue // Extracting the input value from monadic container.
      let monadicOutputValue = transformation inputValue // Transrorming the input value.
      let (Log(newValue, newLogs)) = monadicOutputValue // Extracting the output value of monadic container.
      Log(newValue, logs @ newLogs) // Returning the output value with the new logs added. 
    member x.Return(value) =
      Log(value, [])
    member x.Zero() =
      Log((), [])

  // Creating an instance of logging computation expression builder
  let log = new LoggingBuilder()
  let (>>=) a b = log.Bind (a, b)
  
  // Standard functions
  let incr x = x + 1
  let double x = 2 * x
  let square x = x * x
  let incr2 x = x |> incr |> incr

  // Standard expression
  let expr1 =
    let a = 1
    let b = incr a
    let c = double b
    let d = square c
    d
  
  // Enhanced functions
  let lift name f  = fun x ->
    let y = f x
    let msg = sprintf "%s called with %A returned %A." name x y
    Log(f x, [msg])
  
  // Lifted functions
  let incrM = lift "Increment" incr
  let incr2M = lift "Increment twice" incr2
  let mapM a = lift "MapM" (List.map a)
    
  let expr2 = log {
      let a = 1
      let! b = incrM a
      let double = lift "Double" double // Shadowed
      let! c = double b
      let d = square c
      return d
    } 

  let expr3 = log {
      let a = [1..5]
      let! b = mapM (double) a
      let! c = mapM (incr) a
      return b
    }


  
  // Helper function for writing log message to monadic container
  let logMessage s = Log((), [s])

  
  // Standard function to write to console
  let write (s : string) = System.Console.WriteLine(s)
  // It's monadic version
  let monadicWrite s = log {
    do! logMessage("About to use write() function with this parameter: " + s)
    // Needs Zero() method. Without Zero() method we need to call Return() by return WriteLine().
    write s
    do! logMessage("Did it.")
    }
  // It's desugared version. It is identical to the monadic version above.
  let desugaredMonadicWrite s =
    log.Bind (logMessage("About to use write() function with this parameter: " + s), fun () ->
      write s
      log.Bind (logMessage("Did it."), fun () ->
        log.Return ()
      )
    )
    
  // Standard function to read from console
  let read() = System.Console.ReadLine()
  // It's monadic version   
  let monadicRead () = log {
    do! logMessage("About to use read() function.")
    let s = read()
    do! logMessage("Function read() read this: " + s)
    return s }
  // Desugared version. It is identical to the monadic version above.
  let desugaredMonadicRead () =
    log.Bind ( logMessage("About to use read() function."), fun () ->
      let s = read()
      log.Bind ( logMessage("Function read() read this: " + s), fun () ->
        log.Return s
      )
    )
  
  // Some standard sequential code
  let testIt () =
    do write "Enter your name"
    let name = read()
    "Hello " + name + "!"

  // It's monadic version
  let monadicTestIt () = log {
    do! logMessage "Function monadicTestIt()."
    do! monadicWrite "Enter your name"
    let! name = monadicRead()
    return "Hello " + name + "!" }
  // Desugared monadic version. It uses the desugared function but it could use the sugared ones too.
  let desugaredMonadicTestIt () =
    log.Bind (logMessage "Function monadicTestIt().", fun () ->
      log.Bind (desugaredMonadicWrite "Enter your name", fun () ->
        log.Bind (desugaredMonadicRead(), fun name ->
          log.Bind (logMessage "Finished, now just return the value.", fun () ->
            log.Return ("Hello " + name + "!")
          )
        )
      )
    )

  // Evaluate the standard expression
  let res = testIt ()
  // Evaluate the monadic expression
  let monadicRes = monadicTestIt ()
  // Evaluate the desugared monadic expression
  let monadicRes = desugaredMonadicTestIt ()

  // Another test
  let anotherTestIt =
    let func x = 1.0 / x
    let list = [1.0; 2.0; 4.0]
    list |> List.map func
  // Another test. Monadic, desugared.
  let logIt (s : string) : M<unit> =
    Log((), [s])
  let monadicAnotherTestIt =
    let func x =
      log.Bind (logIt ("Function manadicAnotherTestIt called with argument " + x.ToString()), fun () ->
        let result = 1.0 / x
        log.Bind (logIt ("Result = " + result.ToString()), fun () ->
          log.Return (result)
        )
      )
    let list = [1.0; 2.0; 4.0]
    list |> List.map func

  let (++) (a:int) (b:int) = log {
    do! logMessage ("Adding " + a.ToString() + " and " + b.ToString() + ".")
    return a + b }
  
  let myFold folder state list = log {
    let! res = list |> List.fold folder state
    return res }

  let resAdd = log {
    let a = 1
    let b = 2
    let c = a + b
    let! d = a ++ b
    return c ++ d }

// Taken from http://fsharpforfunandprofit.com/posts/computation-expressions-intro/
module StateMonad =
  type M<'a, 's> = M of ('s -> 'a * 's) // Definition of monadic container

  // Helper functions
  let runState (M m) s = m s
  let getState = M (fun s -> (s, s))
  let putState s = M (fun _ -> ((), s))

  type StateBuilder() =
    member this.Return(a) = 
      M (fun s -> (a, s))
    member this.MyReturn v =
      M (fun s -> (v, s))
    member this.Bind(m, k) =
      M (fun s -> 
        let (a, s') = runState m s 
        runState (k a) s')
    member this.MyBind (mv, transformation) = ()
    member this.ReturnFrom (m) = m

  let state = new StateBuilder()

  let DoSomething counter = 
    printfn "DoSomething. Counter=%i " counter
    counter + 1

  let FinalResult counter = 
    printfn "FinalResult. Counter=%i " counter
    counter

  // convert old functions to "state-aware" functions
  let lift f = state {
    let! s = getState 
    return! putState (f s)
    }

  // new functions
  let DoSomething' = lift DoSomething
  let FinalResult' = lift FinalResult

// Exercise: How well do you understand?
// At the end of the blogpost http://fsharpforfunandprofit.com/posts/computation-expressions-bind/
module Exercise1 =
  // Part 1 - create a workflow
  // Parses string to int option
  let strToInt s =
    let (success, value) = System.Int32.TryParse s // F# converts the function returning byRef to function returning tuple.
    if success then Some value else None
  // Computation expression builder
  type MaybeBuilder () =
    member this.Bind(x, f) =
      match x with
      | Some x -> f x
      | None -> None
    member this.Return x = Some x
  let myWorkflow = new MaybeBuilder()
  let stringAddWorkflow x y z =
    myWorkflow {
      let! a = strToInt x
      let! b = strToInt y
      let! c = strToInt z
      return a + b + c }
  let res1 = stringAddWorkflow "1" "2" "3"
  let res2 = stringAddWorkflow "a" "2" "3"

  // Exercise form http://fsharpforfunandprofit.com/posts/computation-expressions-bind/
  // Part 2 -- create a bind function
  let strAdd s i =
    match strToInt s with
    | Some j -> Some (i + j)
    | None -> None
  let (>>=) m f =
    match m with
    | Some x -> f x
    | None -> None
  let good = strToInt "1" >>= strAdd "2" >>= strAdd "3"
  let bad = strToInt "1" >>= strAdd "xyz" >>= strAdd "3"
    
module MonadicModule =
  type M<'a> = M of 'a * string list
  type MyBuilder () = 
    member this.Bind (m, f) =
      let (M(input, list)) = m
      let (M(output, item)) = f input
      M(output, item @ list)
    member this.Return (x) = M(x, [])
    member this.ReturnFrom(m) = m

  let my = MyBuilder ()
  let log s = M((), [s])
  
  let lift s f x = M(f x, [s])
  let succ a = a + 1
  let succM = lift "Function succM()." succ
  
  let add a b = a + b
  let addM = lift "Function addM()." add

  let test = succMon 10

  let res = my {
    let a = 10
    let! b = inc a
    let! c = inc 20
    let! res = inc c
    return res }

  // Monadic type
  type MT<'T> = Log of 'T * list<string>
   // Computation expression buider. (Definition of the two essential modad function bind and return)
  type LoggingBuilder () =
    member x.Bind(monadicInputValue, transformation) =
      let (Log(inputValue, logs)) = monadicInputValue // Extracting the input value from monadic container.
      let monadicOutputValue = transformation inputValue // Transrorming the input value.
      let (Log(newValue, newLogs)) = monadicOutputValue // Extracting the output value of monadic container.
      Log(newValue, logs @ newLogs) // Returning the output value with the new logs added. 
    member x.Return(value) =
      Log(value, [])
    member x.Zero() =
      Log((), [])
  // Creating an instance of logging computation expression builder
  let log = new LoggingBuilder()
  // Helper function for writing log message to monadic container
  let logMessage s = Log((), [s])
  let add a b = log {
    do! logMessage "add"
    return a + b }
  let res = log {
    let a = 1
    let b = 2
    let! c = add a b
    return (add c 1) }


// Railway oriented programming
type Result<'T> =
  | Success of 'T * string
  | Failure of string

let divide a b = a / b
let modulo a b = a % b

let divideM a b =
  if b = 0 then Failure (sprintf "You are dividing %A by zero." a) else Success (divide a b, sprintf "Ok, you are dividing %A by %A." a b)

let map f m = // Also called "lift".
  match m with
  | Success (x, msg) -> Success (f x, msg)
  | Failure msg -> Failure msg

let bind f m =
  match m with
  | Success (x, msg) -> f x
  | Failure msg -> Failure msg

let fork f x =
  Success (f x, "Function f called within fork.")

// Wraps orignal type into monadic type
let wrap x = Success (x, "")

let modulo13 a = modulo a 13
let divide2 a = divideM a 2
let divide0 a = divideM a 0

let list = [3..-1..1]
let actions = list |> List.map divideM
let chainableActions = actions |> List.map bind
let chained = chainableActions |> List.reduce (>>)

3 |> wrap |> chained

let getRes =
  wrap >> map (modulo 13) >> bind (divideM 2) >> bind (divideM 2) 

let res = getRes 10
 
let myIf a b c = if a then b else c

let myWhile a b = while a do b

//  input
//  |> wrap
//  |> map modulo13
//  |> bind divide2
//  |> bind divide0



// =====================================
// Playground for ideas from the article
// "Monads for functional programming"
// by Philip Wadler
// =====================================
module Wadler =
  type Term =
    | Con of int
    | Div of Term * Term

  // Two example terms
  let answer = Div (Div (Con 1972, Con 2), Con 23)
  let error = Div (Con 1, Con 0)

// ---- Variation zero: The basic evaluator ----
// Recursive type for algebraic expression
module Wadler0 =
  open Wadler
  // Function to evaluate the term
  let rec eval term =
    match term with
    | Con a -> a
    | Div (t, u) -> eval t / eval u

  eval answer
  eval error

// ---- "Variation one: Exceptions" ----
module Wadler1 =
  open Wadler

  type Exception = string
  type M<'a> =
    | Return of 'a
    | Raise of Exception

  let rec eval term =
    match term with
    | Con a -> Return a
    | Div (t, u) ->
      match eval t with
        | Raise e -> Raise e
        | Return a ->
          match eval u with
            | Raise e -> Raise e
            | Return b ->
              if b = 0
                then Raise "Divide by zero."
                else Return (a / b)
  eval answer
  eval error

// ---- "Variation two: State" ----
module Wadler2 =
  open Wadler

  type State = int
  type M<'a> = State -> ('a * State)

  let rec eval term : M<int> =
    match term with
    | Con a -> (fun x -> (a, x))
    | Div (t, u) ->
      fun x ->
        let (a, y) = eval t x
        let (b, z) = eval u y
        (a / b, z + 1)
  //      let (a, y) = eval t 0
  //      let (b, z) = eval u 0
  //      (a / b, x + y + z + 1)
    
  eval answer 0
  eval (Div (Div (Con 1,Con 1), Div (Con 1,Con 1))) 0 // (1 / 1) / (1 / 1) gives (1, 3) which is ok.

// ---- "Variation three: Output" ----
// This could also be called logging.
module Wadler3 =
  open Wadler
  type Output = string
  type M<'a> = Output * 'a

  // Helper function: Prints the term on LHS and the value on the RHS.
  let line (term:Term) (value:int) = sprintf "eval (%A) <= %A\n"  term value

  // Eval function converts a term to a monadic type, i.e. tuple of output and value.
  let rec eval term : M<int> =
    match term with
    | Con a -> (line term a, a)
    | Div (t, u) ->
      let (out, a) = eval t
      let (out2, b) = eval u
      let res = a / b
      (out + out2 + line term res, res)

  eval answer

// Monadic approach
// Monad consist of triple (M<'a>, unit, (*))
// M is called type constructor
// unit is of type 'a -> M<'a>
// bind operator is of type M<'a> -> ('a -> M<'a>) -> M<'a>, bind is also called "shove" informally

// ---- "Variation zero, revisited: The basic evaluator" ----
module Wadler0r =
  open Wadler

  // Identity monad
  // M is just a
  type M<'a> = 'a
  // Unit is the identity function.
  let unit (a:'a) : M<'a> = a
  // Bind operation is just normal function application.
  // The (>op) custom operator is left asociative. 
  let (>>=) (m:M<'a>) (f:'a->M<'b>) : M<'b> = f m

  // This evaluator is the same for all three variations
  let rec eval (term:Term) : M<int> =
    match term with
    | Con a -> unit a
    | Div (t, u) -> (eval t) >>= (fun a -> (eval u) >>= (fun b -> unit (a / b)))

  eval answer // Returns 42.
  eval error  // Throws an exception.

// ---- "Variation one, revisited: Exceptions" ----
module Wadler1r =
  open Wadler

  type Exception = string
  type M<'a> =
    | Return of 'a
    | Raise of Exception
  // Unit. I a is given just "wrap" it.
  let unit (a:'a) : M<'a> = Return a
  // Bind operation
  // The (>op) custom operator is left asociative. 
  let (>>=) (m:M<'a>) (f:'a->M<'b>) : M<'b> =
    match m with
    | Raise e -> Raise e // Reraise exception
    | Return a -> f a // Apply function to unwraped value

  // This evaluator is the same for all three variations
  let rec eval (term:Term) : M<int> =
    match term with
    | Con a -> unit a
    | Div (t, u) -> (eval t) >>= (fun a -> (eval u) >>= (fun b -> if b = 0 then Raise (sprintf "Dividing %A by zero" a) else unit (a / b)))

  eval answer // Returns 42.
  eval error  // Throws an exception.

// ---- "Variation two, revisited: State" ----
module Wadler2r =
  open Wadler

  type State = int
  type M<'a> = State -> ('a * State)

  // Unit. I a is given just "wrap" it.
  let unit (a:'a) : M<'a> = fun state -> (a, state)
  // Bind operation
  // The (>op) custom operator is left asociative. 
  // KEY: m updates state and applied f also updates state
  let (>>=) (m:M<'a>) (f:'a->M<'b>) : M<'b> =
    let resFunc state =
      // Unwrap and update state
      let (value, newState) = m state
      // Apply function to value
      let resM = f value
      // Update the new state
      let res = resM newState
      res
    // Return the function
    resFunc
 

  // This evaluator is the same for all three variations
  let rec eval (term:Term)  : M<int> =
    match term with
    | Con a -> unit a
    | Div (t, u) -> (eval t) >>= (fun a -> (eval u) >>= (fun b ->
        fun state -> (a / b, state + 1)
      ))

  eval answer 0 // Returns (42, 2)
  eval error 0  // Throws an exception.
  eval (Div (Div (Con 1,Con 1), Div (Con 1,Con 1))) 0 // (1 / 1) / (1 / 1) gives (1, 3) which is correct.

// ---- "Variation three, revisited: Output" ----
// (I would also call it "monadic logging")
// ---- "Variation two, revisited: State" ----
module Wadler3r =
  open Wadler
  type Output = string
  type M<'a> = Output * 'a

  // Unit. I a is given just "wrap" it.
  let unit (a:'a) : M<'a> = ("", a)
  // Bind operation
  // The (>op) custom operator is left asociative. 
  // KEY: m updates state and applied f also updates state
  let (>>=) (m:M<'a>) (f:'a->M<'b>) : M<'b> =
    // Unwrap and update state
    let (output, value) = m
    // Apply function to value
    let (newOutput, newValue) = f value
    // Update the new state
    let res = (output + newOutput, newValue)
    res
  
  // Helper function: Prints the term on LHS and the value on the RHS.
  let line (term:Term) (value:int) = sprintf "eval (%A) <= %A\n"  term value

  // This evaluator is the same for all three variations
  let rec eval (term:Term)  : M<int> =
    match term with
    | Con a -> (line term a, a)
    | Div (t, u) -> (eval t) >>= (fun a -> (eval u) >>= (fun b -> (line term (a / b) , a / b)))

  eval answer // Returns (42, 2)
  eval error  // Throws an exception.
  eval (Div (Div (Con 1, Con 1), Div (Con 1, Con 1))) // (1 / 1) / (1 / 1) gives (1, 3) which is correct.




