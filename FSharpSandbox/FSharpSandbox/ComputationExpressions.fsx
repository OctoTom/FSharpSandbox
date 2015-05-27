// Computation expressions (monads)
module ComputationExpressions = // Option-returning function
  let tryDecr x n = 
    printfn "Conditionally decrementing %A by %A" x n
    if x > n then
      printfn "Condition met."
      Some (x - n)
    else
      printfn "Condition not met."
      None
  // Builder
  type MaybeBuilder() =
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
  let maybe = new MaybeBuilder()
  // Define an function using the instance of maybe computation expression
  let maybeDecr x =
    maybe {
      let! y = tryDecr x 10
      let! z = tryDecr y 30
      let! t = tryDecr z 50
      return t
    }
  let maybeDecr_v2 x =
    maybe.Bind (tryDecr x 10, fun y ->
      maybe.Bind (tryDecr y 30, fun z ->
        maybe.Bind (tryDecr z 50, fun t ->
          maybe.Return t)))  

  // Call the function with various values
  let res1 = maybeDecr 100
  let res1_v2 = maybeDecr_v2 100
  let res2 = maybeDecr 30
  let res2_v2 = maybeDecr_v2 30
  let res3 = maybeDecr 10
  let res3_v2 = maybeDecr_v2 10

  // Monad primer from F Sharp Programing / Computation Expression (WikiBooks)
  // 1. Everyday sequential code
  module Sequential =
    // Definitions
    let readLine() =
      System.Console.ReadLine()
    let printLine(s) =
      printfn "%s" s
    // Sequential code
    let expr1 =
      printLine "What is yout name?"
      let name = readLine()
      printLine ("Hello, " + name + "!")

  // 2. Sequential code writen in functional style. ("If you can understand this much, than you can understand any monad.")
  // Definitions
  module SequentialWithFunctional =
    let readLine(f) =
      f(System.Console.ReadLine())
    let printLine(s, f) =
      f(printfn "%s" s)
    // Chained functions
    let expr = 
      printLine("What is your name?", fun () ->
        readLine(fun name ->
          printLine("Hello, " + name + "!", fun () ->
            ()
          )
        )
      )

  // 3. Define my own Bind function
  module DefineOwnBindMethod =
    let bind (x, f) = f x
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

    // Sugared monad-style expression
    let exp3 =
      standard { // Sugared
        let! a = 2
        let! b = a + 3
        let! c = b + 5
        return a + b + c
      }

  // 3. Define my own Bind function
  let addThreeNumbers() =
    let bind(input, rest) =
      match System.Int32.TryParse(input()) with // F# translates .NET functions with "out" parameters to functions returning a tuple. (http://hestia.typepad.com/flatlander/2010/07/f-pattern-matching-for-beginners-part-2-decomposition.html)
      | (true, n) when n >= 0 && n <= 100 -> Some(n)
      | _ -> None
    let createMsg msg =
      fun () ->
        printf "%s" msg
        System.Console.ReadLine()
    bind(createMsg "#1: ", fun x ->
      bind(createMsg "#2: ", fun y ->
        bind(createMsg "#3: ", fun z -> Some(x + y + z))))
  let result = addThreeNumbers()

  // My experiments with monadic style
  module MyMonadicExperiments =
    // 1. Sequential
    let exp1 =
      let a = 2
      let b = a + 3
      let c = b + 5
      a + b + c
    // 2. Modnadic
    type VerboseStandardBuilder() = // Definition of monad
      member this.Delay(f) =
        printfn("Function Delay called.")
        f()
      member this.Bind(x, f) =
        printfn "You just bound %A to %A" x f
        f(x) 
      member this.Return(x) =
        printfn("Function Return called.")
        x
    let standard = VerboseStandardBuilder()
    // 2a. De-sugared monad-style expression
    let exp2 =
      standard.Delay(fun () ->
        standard.Bind(2, fun a ->
          standard.Bind(a + 3, fun b ->
            standard.Bind(b + 5, fun c ->
              standard.Return(a + b + c)
            )
          )
        )
      ) 
    // 2.b Sugared monad-style expression
    let exp3 =
      standard { // Sugared
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
          
// LOGGING COMPUTATION EXPRESSION //
module LoggingComputationExpression =
  // Monadic type
  type M<'T> = Log of 'T * list<string>
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
