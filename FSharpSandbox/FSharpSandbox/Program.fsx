// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open System.IO // Equivalent of C# #using System.IO

module Comments =
  /// <summary>This is <c>function</c> summary.</summary>
  /// <param name="a">First number</param>
  /// <param name="b">Second number</param>
  /// <returns>The sum the two numbers</returns>
  /// <exception cref="System.ArgumentNullException">This function does not throw any exception.</exception>
  let myFunction a b =
    a + b
  /// This is value res
  let res = myFunction 2 3

module Shadowing =
  // Shadowing the variable arg
  let myFuncWithShadowing arg = 
    let arg = 2 * arg
    arg + 1
  // means this
  let myFuncWithoutShadowing arg = 
    let arg2 = 2 * arg
    arg2 + 1

// Explicitely specified type of argument (needed at times)
module ExplicitelyTypedArgument =
  let toHackerTalk (phrase : string) =
    phrase.Replace('t', '7').Replace('o', '0')
  let res = toHackerTalk "thomas"

// Function defined within function
module FunctionWithinFunction =
  let quadruple x =     // Outer function binding
    let double x =    // Inner function binding
      x * 2         // Inner function return value
    double(double(x)) // Outer function return value

// Higher order function
module HigherOrderFunction =
  let applyTwice func arg =
    func (func arg)
  // First order function
  let sqr x =
    x * x
  // Calling higher order function with first order function and value as arguments.
  let res1 = applyTwice sqr 3
  // Partial application
  let toPowerFour = applyTwice sqr
  let res2 = toPowerFour 3

// Lambda (anonymous) function 
module AnonymousFunctions =
  // Lambdas are are like literals of function type
  let res1 = (fun x -> x + 1) 12
  let res2 = (fun x y -> x + y) 2 4
  let add = (fun x y -> x + y)
  let res3 = add 3 4

// List
module Lists =
  // Initialize list
  let myList = [2; 4; 6; 8]
  let oneToTen = [1..10]
  let oddOneToTen = [1..2..10]
  let cubes = List.init 4 (fun index -> System.Math.Pow((float)index, 3.0));
  // Mapping
  let squares = List.map (fun x -> x * x) oddOneToTen

module PipeOperator =
  // First filter the list than double it.
  let result1 =
    List.map (fun x -> x * 2) (List.filter (fun x -> x % 2 = 0) [0..10]) 
  // With pipeline
  let result2 = 
    [0..10]
    |> List.filter (fun x -> x % 2 = 0)
    |> List.map (fun x -> x * 2) 

// Parsing simple csv
module ParsingCSV =
  //"Date,Open,High,Low,Close,Volume,Adj Close"
  let stockData =
      [ 
        "2012-03-30,32.40,32.41,32.04,32.26,31749400,32.26";
        "2012-03-29,32.06,32.19,31.81,32.12,37038500,32.12";
        "2012-03-28,32.52,32.70,32.04,32.19,41344800,32.19";
        "2012-03-27,32.65,32.70,32.40,32.52,36274900,32.52";
        "2012-03-26,32.19,32.61,32.15,32.59,36758300,32.59";
        "2012-03-23,32.10,32.11,31.72,32.01,35912200,32.01";
        "2012-03-22,31.81,32.09,31.79,32.00,31749500,32.00";
        "2012-03-21,31.96,32.15,31.82,31.91,37928600,31.91";
        "2012-03-20,32.10,32.15,31.74,31.99,41566800,31.99";
        "2012-03-19,32.54,32.61,32.15,32.20,44789200,32.20";
        "2012-03-16,32.91,32.95,32.50,32.60,65626400,32.60";
        "2012-03-15,32.79,32.94,32.58,32.85,49068300,32.85";
        "2012-03-14,32.53,32.88,32.49,32.77,41986900,32.77";
        "2012-03-13,32.24,32.69,32.15,32.67,48951700,32.67";
        "2012-03-12,31.97,32.20,31.82,32.04,34073600,32.04";
        "2012-03-09,32.10,32.16,31.92,31.99,34628400,31.99";
        "2012-03-08,32.04,32.21,31.90,32.01,36747400,32.01";
        "2012-03-07,31.67,31.92,31.53,31.84,34340400,31.84";
        "2012-03-06,31.54,31.98,31.49,31.56,51932900,31.56";
        "2012-03-05,32.01,32.05,31.62,31.80,45240000,31.80";
        "2012-03-02,32.31,32.44,32.00,32.08,47314200,32.08";
        "2012-03-01,31.93,32.39,31.85,32.29,77344100,32.29";
        "2012-02-29,31.89,32.00,31.61,31.74,59323600,31.74"; ]
  // Get date of day with highest absolute difference between open and close price.
  let result =
    stockData
    |> List.map (fun (line:string) -> line.Split([|','|]))
    |> List.maxBy (fun x -> abs(float x.[1] - float x.[4]))
    |> (fun x -> x.[0])

// Records
module Records =
  // Define record type
  type Book = 
    { Name: string;
      AuthorName: string;
      Rating: int;
      ISBN: string }
  // Create instance of Book
  let expertFSharp = 
    { Name = "Expert F#";
      AuthorName = "Don Syme, Adam Granicz, Antonio Cisternino";
      Rating = 5;
      ISBN = "1590598504" }
  // Use a field of a record
  printfn "I give this book %d stars out of 5!"
    expertFSharp.Rating
  // Create new record based on existing record
  let partDeux = { expertFSharp with Name = "Expert F# 2.0" }
  // Print the name of the new book
  printfn "The name of the new book is %A"
    partDeux.Name

// Optional type
module OptionalType = 
  let rating1 : int option = None     // No rating
  let rating2 : int option = Some 5   // Five stars
  let writeRating rating = // Function that consumes the rating
    match rating with
    | Some x -> printfn "Rating is %A" x
    | None -> printfn "There is no rating"
  writeRating rating1 |> ignore // Can ignore the return value of writeRating
  writeRating rating2 |> ignore // Can ignore the return value of writeRating

// Discriminated union
module DiscriminatedUnion =
  type Communication =
    | Call of int
    | Message of string
    | Flag
  let writeCommunication communication = // Function that consumes the discriminated union
    match communication with 
    | Call x -> printfn "The call number is %A." x
    | Message x ->  printfn "The message text is %A." x
    | Flag -> printfn "The flag is up!"
  writeCommunication (Call 123)               // Use the function with call as an argument
  writeCommunication (Message "Hello world!") // Use the function with message as an argument
  writeCommunication Flag                     // Use the function with flag as an argument
  // Printable discriminated union
  type Target =
    | Fund of string
    | Spread of string * string
    // Creates target name. E.g. for fund the name is "ULPIX", for spread the name is "ULPIC-UJPIX".
    override this.ToString () =
      match this with
      | Fund ticker -> ticker
      | Spread (long, short) -> long + "-" + short

// Generic discriminated union
module GenericDiscriminatedUnion =
  type 'T SingleOrPair =
    | Single of 'T
    | Pair of 'T * 'T
  let val1 = Single 1.0
  let val2 = Pair ("Abc", "Def")
  // let val3 = Pair ("Abc", 1.0) // would generate compile error

// Enumerations
module Enumerations =
  type DayType =
    | weekDay = 0
    | weekendDay = 1
    | everyDay = 2
  let myDayType = DayType.weekDay
  let isWeekDay a =
    match a with
    | DayType.weekDay -> true
    | _ -> false
  let res = isWeekDay myDayType
  

// Left assotiation of application of two arguments
module FunctionApplication =
  let add x y =
    x + y
  let res1 = add 1 2 // Conventional
  let res2 = ((add 1) 2) // Means this

module PartialApplication =
// Partial application (function specialization)
  let div x y =
    x / y
  let inverseValue = // Function "inverseValue x" is defined as "div 1.0 x"
    div 1.0
  inverseValue 5.0

  // Partial application (filter specialization)
  let filterEven = Seq.filter(fun x -> x % 2 = 0)
  printf "There are %A even numbers in the sequence."
    ([0..10] |> filterEven |> Seq.length) 

// Swapping arguments of f from f a b to f b a.
module SwapArguments =
  let subtract x y = x - y
  let res1 = subtract 2 1
  let swapArgs f x y = f y x
  let res2 = swapArgs subtract 2 1
  let decrement = swapArgs subtract 1
  let res3 = decrement 10

// Pattern matching
module PatternMatching =
  let input = [ (1., 2., 0.); (2., 1., 1.); (3., 0., 1.) ]
  let rec search lst =
    match lst with
    | (1., _, z) :: tail -> 
        printfn "found x=1. and z=%f" z; search tail
    | (2., _, _) :: tail -> 
        printfn "found x=2."; search tail
    | _ :: tail -> search tail
    | [] -> printfn "done."
  search input

  // Active pattern
  let (|Norm|) (a:float, b:float, c:float) = 
      sqrt(a*a + b*b + c*c)
  let writeVector x =
    match x with
    | Norm(1.) -> printfn "Unit vector %A found!" x
    | Norm(n) -> printfn "General vector %A with norm %f." x n
  writeVector (1., 0., 0.) |> ignore
  writeVector (1., 1., 0.) |> ignore

  // Classification with Patterns
  let (|Vector|Versor|) (a:float, b:float, c:float) = // Original implementation with if-then-else
      if sqrt(a*a + b*b + c*c) = 1. then Versor 
          else Vector
  let (|Vector2|Versor2|) (a:float, b:float, c:float) = // My implementation with pattern matching
    match sqrt(a*a + b*b + c*c) with
    | 1.0 -> Versor2 
    | _ -> Vector2 // Incomplete patern matching
  let writeVector2 x =
    match x with
    | Versor2 -> printfn "Versor found!"
    | Vector2 -> printfn "It is just a vector."
  writeVector2 (1., 0., 0.) |> ignore
  writeVector2 (1., 1., 0.) |> ignore
  // Keyword "function"  
  let m1 x =
    match x with
    | 0 -> "Zero"
    | _ -> "Number"
  let m2 = function
    | 0 -> "Zero"
    | _ -> "Number"


module ActivePatternMatching =
  // Define active pattern
  let (|F2C|) fahr = 
    (fahr - 32.0) / 1.8
  // Common function
  let F2C fahr = 
    (fahr - 32.0) / 1.8
  // Pttern matching using active pattern
  let test (fahr:float) =
    match fahr with
    | F2C c when c > 30.0 -> printfn "It's hot %f." c
    | F2C c when c > 10.0 -> printfn "It's temperature %f." c
    | _ -> printfn "Better not ask!"
  let test2 (fahr:float) =
    let aux = F2C fahr 
    match aux with
    | c when c > 30.0 -> printfn "It's hot %f." c
    | c when c > 10.0 -> printfn "It's temperature %f." c
    | _ -> printfn "Better not ask!"
  let res1 = test 100.0
  let res2 = test 60.0
  let res3 = test -100.0
  let res1b = test2 100.0
  let res2b = test2 60.0
  let res3b = test2 -100.0

  // Another example of active pattern matching
  let (|DaysToYears|) (days:int) =
    (float)days / 365.0
  let congrats days =
    match days with
    | 0 -> printfn "Welcome to the world!"
    | 1 -> printfn "Happy first day, infant!"
    | 10000 -> printfn "One thousand days on earth!"
    | DaysToYears 1.0 -> printfn "Happy first birthday, yearling!"
    | DaysToYears x -> printfn "Happy %f. birthday!" x
    | x -> printfn "Nothing special this day"
  let res4 = congrats 0
  let res5 = congrats 1
  let res6 = congrats 365
  let res7 = congrats (15 * 365)
  let res8 = congrats 10000
  let res9 = congrats 10001

  // Just another example
  let (|Exp|) (x:float) =
    exp x
  let (|Sin|) (x:float) =
    sin x
  let testNumber x =
    match x with
    | Exp 1.0 -> printfn "Exp of %f gives 1." x     // Exp(x) matches 1.0
    | Sin 1.0 -> printfn "Sin of %f is 1." x // Sin(x) matches 1.0 
    | _ -> printfn "No special number."             // Matches all
  let res10 = testNumber 0.0
  let res11 = testNumber (System.Math.PI / 2.0)
  let res12 = testNumber 365.0

module Sequences = 
  // Sequence (infinite list)
  let N = seq {
      let n = ref 0
      while true do
          yield !n
          n := !n + 1
  }
  N |> Seq.take(10) |> Seq.iter 
      (fun v -> printfn "%d" v)

  // Sequence
  let seq1 = seq { 0 .. 10 .. 100 }
  let seq2 = seq { for i in 1..10 do yield i * i }

  // Sequence of indexes in rectangular matrix
  let (height, width) = (3, 3)
  let result =
    seq {
      for row in 0 .. width - 1 do
        for col in 0 .. height - 1 do
          yield (row, col, row*width + col)
    }
    |> Seq.take 5
    |> Seq.toList 

// Tuples
module Tuples =
  let myTuple = (1, 2.0)
  let swapElements (a, b) =
    (b, a)
  let res1 = swapElements myTuple
  let res2 = swapElements ("abc", 1)

  let t = (1, 2.0, "three")
  let t1, _, _ = t // Functions fst and snd work only with pairs
  let (_, t2, t3) = t


  // Do bindings
  do printfn "Abc"
  printfn "Abc" // Do keyword can be omitted
  do 12 |> ignore


    

module Memoization =
  let memoize(f) =
    let cache = new System.Collections.Generic.Dictionary<_, _>()
    fun x ->
      match cache.TryGetValue(x) with
      | true, v -> v
      | _ ->
        let v = f(x)
        cache.Add(x, v)
        v
  let myFunc (x, y) =
    printfn "MyFunc() called with numbers %A and %A." x y
    x + y
  let myFuncMemoized = memoize myFunc
  let res1 = myFuncMemoized (2, 3)
  let res2 = myFuncMemoized (2, 4)
  let res3 = myFuncMemoized (2, 3)


module Miscelaneous =
  // .NET functions involving out parameters translates to F# functions retrurning tuple.
  let myTryParse (s : string) = // val myTryParse : s:string -> bool * int
    System.Int32.TryParse(s)

module UnitAcceptingFunction =
  let a = 13
  let b() = 13
  let c = b
  let d = b()
  let e = c()
// End of module UnitAccepting Function





#if INTERACTIVE
#load "../packages/FSharp.Charting.0.84/FSharp.Charting.fsx"
#endif

open FSharp.Charting
//open System
//module aaa = 
// Drawing graph of a 'square' function 
Chart.Line [ for x in 1.0 .. 100.0 -> (x, x ** 2.0) ]





  
// Charting
module MyCharting =
  #if INTERACTIVE
  #r "../packages/FSharp.Charting.0.84/lib/net40/FSharp.Charting.dll" 
  #load "../packages/FSharp.Charting.0.84/FSharp.Charting.fsx"
  #endif
  open FSharp.Charting
  // As argment of function
  Chart.Line([ for x in 1.0 .. 10.0 -> (x, x ** 2.0) ], Name = "Aaa")
  // List piped into the chart
  [ for x in 1.0 .. 10.0 -> (x, x ** 2.0) ] |> Chart.Line


module ScientificAndNumericalComputing = // www.tryfsharp.org -> Learn -> Scientific..
  // Centigrade to Fahrenheit and vice versa conversion
  module SimpleStuff =
    let centToFahr t = 1.8 * t + 32.0
    let fahrToCent t = (t - 32.0) / 1.8
    let res1 = centToFahr 0.0
    let res2 = fahrToCent 0.0
    let res3 = centToFahr (fahrToCent 23.1)
    let res4 = fahrToCent (centToFahr 11.4)
    // Defining the conversion as linear function
    let linearFunction a (x:float) b = a * x + b
    let centToFahrLin t = linearFunction 1.8 t 32.0 
    let res5 = centToFahrLin 1.0

  // Units of measurements
  module UnitsOfMeasurements =
    open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
    let g = 9.8<m/s^2>
    let mass = 100.0<kg>
    let force : float<N> = g * mass
    // Conversion
    [<Measure>]
    type km
    let m2km (d:float<m>) = d * 0.001<km/m>
    let km2m (d:float<km>) = d * 1000.0<m/km>
    let length = 1.0<km> + m2km 10.0<m>

  // Roll a die
  module RandomNumbers =
    let generator = System.Random()
    let rollDie() = generator.Next(6) + 1
    let res = rollDie()
  
  // Create a histogram of rolls
  module FSharpCharting=
    open FSharp.Charting
    let generator = System.Random()
    let histogram n =
      let gen = System.Random()
      let rollDie() = generator.Next(6) + 1
      let rolls = [for i in 1 .. n -> rollDie()]
      let counts = Seq.countBy (fun i -> i) rolls
      Chart.Line(counts, Name = gen.ToString())
    // Draw it
    histogram 1000

  // Factorial using recurse
  let rec factorial n =
    match n with
    | 0 -> 1
    | n when n > 0 -> n * factorial(n - 1)
    | _ -> failwith "Invalid argument"
  let numbers = [-1 .. 10]
  let res1 = numbers |> List.map factorial

  // Power function using recursion
  let rec power (x:float) n =
    match n with
    | 0 -> 1.0
    | n -> x * power x (n - 1)
  let res2 = numbers |> List.map (power 2.0)

  // Newton's method
  module NewtonMethod =
    let func x =
      exp(x) - 2.0
    let diff f (x:float) h =
      (f(x + h) - f(x - h)) / 2.0 / h
    let rec newtonsMethod f x =
      //let mutable count = 0
      let x_new = x - f(x) / diff f x 1.0e-6 
      match x_new with
      | _ when abs(x_new - x) < 1.0e-6 -> x_new
      | _ -> newtonsMethod f x_new
    let res1 = newtonsMethod func 0.0
    let check = func res1
    // With counter
    let newthonMethodWithCounting f x =
      let rec newton f x n =
        let x_new = x - f(x) / diff f x 1.0e-6 
        match x_new with
        | _ when abs(x_new - x) < 1.0e-6 -> (x_new, n)
        | _ -> newton f x_new (n+1)
      newton f x 0
    let res2 = newthonMethodWithCounting func 0.0

module BasicRecursion =
  let rec fact x =
    match x with
    | 1 -> 1
    | x -> x * fact(x - 1)
  let res = fact 4

module TailRecursion =
  let fact x =
    let rec tailRecFact x acc =
      match x with 
      | 1 -> acc
      | x -> tailRecFact (x - 1) (acc * x)
    tailRecFact x 1
  let res = fact 4
  

// Closure
module Closure =
  let myPower alpha =
    let f x = x ** alpha
    f
  let cube = myPower 3.0 // creates closure - function myPower with parameter alpha fixed
  let res = cube 2.0

// Generic function
// Swap function enforcing the tuple of the same types.
module GenericFunctions =
  let swap (a:'T, b:'T) = // 'T means "some type", T means concreate type 
    (b, a)

module LinearAlgebra =
  /// <summary>Sums the values to which given <c>function</c> evaluates.</summary>
  /// <param name="i0">First index</param>
  /// <param name="i1">Last index</param>
  /// <param name="f">Function to be passed the indexes</param>
  /// <returns>The sum of function results</returns>
  /// <exception cref="System.ArgumentNullException">This function does not throw any exception.</exception>
  let mySum i0 i1 f =
    [i0..i1] |> List.fold (fun state item -> state + f item) 0.0
  /// This is value res
  let res = mySum 2 4 (fun x -> 2.0 * (float)x) 
  
  // 
module ComputingPiWithMonteCarlo =
  let computePi n =
    let generator = System.Random()
    let randomPoints n =
      List.init n (fun i -> (generator.NextDouble(), generator.NextDouble())) 
    let pointFallsIntoCircle (x, y) =
      match sqrt(x * x + y * y) with
      | r when r < 1.0 -> 1.0
      | _ -> 0.0
    let realizations = (randomPoints n) |> List.map pointFallsIntoCircle
    4.0 * (realizations |> List.average)
  let Pis = List.init 10 (fun i -> computePi 10000)

module EagerListVsLazySeq =
  let myInitFun i =
    match i with
    | 7 ->
      printfn "Started working."
      System.Threading.Thread.Sleep(2000)
      printfn "Finished."
      7
    | x -> x*x
  let myList = List.init 10 myInitFun
  let myListElem = myList.Item 7
  let mySeq = Seq.init 10 myInitFun
  let mySeqElem = mySeq |> Seq.iteri (fun index item -> printfn "Element %A is %A." index item) 

module LazyKeyWord =
  let a = lazy (1 + 2)
  let b = a
  let c = 2 * a.Value

module PrimeNumberGenerator =
  /// A very naive prime number detector
  let isPrime (n:int) =
     let bound = int (sqrt (float n))
     seq {2 .. bound} |> Seq.forall (fun x -> n % x <> 0)
  // We are using async workflows
  let primeAsync n =
      async { return (n, isPrime n) }
  /// Return primes between m and n using multiple threads
  let primes m n =
      seq {m .. n}
          |> Seq.map primeAsync
          |> Async.Parallel
          |> Async.RunSynchronously
          |> Array.filter snd
          |> Array.map fst
  // Run a test
  primes 1000000 1002000
      |> Array.iter (printfn "%d") 

module SignatureOfListAverageMethod =
  let myAverage = List.average // Requires members (+), DivideByInt, get_Zero
  let average = myAverage [1.0; 2.0]
  //let average = myAverage ["Aaa"; "bbb"] // String does not support get_Zero

module WhichGlobalValuesAreUsedInFunctionBody =
  let increment = 1
  let addIncrement a = a + increment
  let res1 = addIncrement 7
  let addIncrement_v2 a =
    let increment = 2
    addIncrement a // Value increment used in function addIncrement is the original global value 1.
  let res2 = addIncrement_v2 13 // res2 : int = 14

// To demonstrate that the function which generates elements is evaluated every time the sequence is iterated.
module SequenceDoesNotRemembreElements =
  let mySeq = Seq.unfold (fun x -> printfn "%A" x; Some(x, x + 1)) 0
  let res1 = mySeq |> Seq.take 4 |> Seq.toList
  let res2 = mySeq |> Seq.take 4 |> Seq.toList

module BarChart = // From fssnip.net
  #r "System.Windows.Forms.DataVisualization.dll"
  open System.Windows.Forms
  open System.Windows.Forms.DataVisualization.Charting

  // A collection of tuples containing titles and values for the chart
  let data = 
    [ "Africa", 1033043; "Asia", 4166741; "Europe", 732759; 
      "South America", 588649; "North America", 351659; "Oceania", 35838  ]

  // Create a chart containing a default area and show it on a form
  let chart = new Chart(Dock = DockStyle.Fill)
  let form = new Form(Visible = true, Width = 700, Height = 500)
  chart.ChartAreas.Add(new ChartArea("MainAre"))
  form.Controls.Add(chart)

  // Create series and add it to the chart
  let series = new Series(ChartType = SeriesChartType.Bar)
  chart.Series.Add(series)
  // Specify data for the series using data-binding
  series.Points.DataBindXY(data, "Item1", data, "Item2")

module Data =
  #r @"C:\Users\Tomas\SkyDrive\Tech\Projects\FSharpSandbox\packages\FSharp.Data.1.1.10\lib\net40\FSharp.Data.dll"
  open FSharp.Data
  open FSharp.Net
  // Infer type "Stocks" from the file "table.csv".
  type Stocks = CsvProvider<"Data/table.csv", InferRows=10>
  let msft = Stocks.Load("http://ichart.finance.yahoo.com/table.csv?s=MSFT")
  let result = msft.Data |> Seq.averageBy (fun row -> (float) row.Volume)

  //let [<Literal>] RssSample = (Sample RSS feed omitted)
  type Rss = XmlProvider<RssSample>
  let feed = Rss.Parse(Http.Request("http://feeds.bbci.co.uk/news/rss.xml"))
  printfn  "%s" feed.Channel.Title
  for item in feed.Channel.GetItems() do
    printfn " - %s" item.Title

  let fb = FreebaseData.GetDataContext()
  for rel in fb.Society.Religion.Religions |> Seq.take 10 do printfn "%s" rel.Name

  let data = WorldBankData.GetDataContext()
  data
    .Countries."United Kingdom"
    .Indicators."School enrollment, tertiary (% gross)"
  |> Seq.maxBy fst

module Exceptions =
  failwithf "Item '%A' already been added to tree" x
  

module TaylorSeries =
  // Simple factorial for purposes of Taylor series.
  // This version is not tail-recursive.
  let rec factorial x =
    match x with
    | 0 -> 1
    | _ -> x * factorial (x - 1)

  // Taylor series.
  // The function takes a list of derivatives in given point a, the given point a and a point x, where we want to compute the value of the function.
  let tailorSeries (derivatives:float list) (a : float) (x : float) =
    derivatives |> List.mapi (fun i f -> f / (float (factorial i)) * (x - a)**(float i)) 

  let a = 0.0
  // List of derivatives f(a), f'(a), f''(a) of sin(a=0)
  let derivatives = [0.0; 1.0; 0.0; -1.0; 0.0; 1.0; 0.0; -1.0]

  let pi = System.Math.PI

  let approxSin x = tailorSeries derivatives a x |> List.sum

  let result = approxSin (pi / 2.0)

module Primes =
  let isPrime i = [2..(float>>sqrt>>int)i] |> List.forall (fun divisor -> i%divisor <> 0)
  let primes = [2..100] |> List.filter isPrime

module Conversions =
  let myInt1 = int32 "100"
  let myInt1b = int32 "100.1" // System.FormatException
  let myInt2 = int32 59.9 // Truncates to 59.
  let myInt3 = int32 -59.9 // Truncates to -59.
  
module ConversionAndFunctionComposition =
  // 1. Convert to float, 2. Compute sinus in degrees, 3. Round, 4. Convert to int32.
  let myInt = (float >> (fun x -> 100.0 * sin (System.Math.PI * x / 180.0)) >> System.Math.Round >> int32) 30

module RandomPermutation =
  // Based on http://fssnip.net/16
  let shuffle sqn =
    let random = new System.Random()
    let rec recursiveShuffle sqn =
      let randomElement = sqn |> Seq.nth (random.Next(0, Seq.length sqn)) // Random.Next(inclusiveMin, exclusiveMax) 
      let restOfElements = sqn |> Seq.filter (fun e -> e <> randomElement)
      seq {
        yield randomElement
        if not (Seq.isEmpty restOfElements) then
          yield! recursiveShuffle restOfElements 
      }
    recursiveShuffle sqn
  
  // Test the shuffle function
  {1..5} |> shuffle |> Seq.toList

  // Sidenote: that the upper bound in System.Random.Next(int min, int max) is exclusive.
  let random = System.Random() // No need for "new" keyword.
  let randomNums = List.init 20 (fun x -> random.Next(1, 3))

  // Different approach based on http://fssnip.net/2n
  let unsort xs =
    let rand = System.Random()
    xs
    |> Seq.map (fun x -> rand.Next(),x)
    |> Seq.cache
    |> Seq.sortBy fst
    |> Seq.map snd
  
  // Test the unsort function
  {1..5} |> unsort |> Seq.toList

  // How Seq.sortBy behaves when it endountres two identical elements?
  let sqn = [(1, "aaa"); (1, "bbb")] |> List.toSeq
  let res = sqn |> Seq.sortBy fst |> Seq.toList

module StringTools =
  // Convert string to list of chracters and back to string.
  let word = "abcd"
  let stringToList word = List.ofSeq word
  let list = stringToList word
  let listToString list = new System.String (list |> List.toArray)
  let word2 = listToString list


module GroupsProblem =
  /// Creates all possible grouping of n identical elements.
  /// The result is list of lists. The inner list contains number of single elements, pair, triples, quadruples, etc.

  let rec groups n =
    match n with
    | 1 -> [[1]]
    | 2 -> [[2; 0]; [0; 1]]
    | 3 -> [[3; 0; 0]; [1; 1; 0]; [0; 0; 1]]
    | x -> []

  let sumOfGroups list =
    list |> List.mapi (fun i itm -> (i+1, itm)) |> List.sumBy (fun x -> fst x * snd x)
  let checkNumOfElemInGroups list = sumOfGroups list = List.length list    

  let res1 = sumOfGroups [0; 0; 1]
  let res2 = checkNumOfElemInGroups [0; 0; 1]

module PrisonersAndBoxes =
  let fact x = [1..x] |> List.fold (*) 1
  let com n p = fact n / fact p / fact (n-p)
  let com2 n p =
    let p =
      match p with
      | p when p > n / 2 -> n - p
      | _ -> p
    ([n-p+1..n] |> List.map float |> List.fold (*) 1.0) / ([1..p] |> List.map float |> List.fold (*) 1.0)
  let per n = fact n
  let var n p = fact n / fact (n-p)

  let res1 = com 10 6
  let res2 = [1..10] |> List.map (fun item -> com2 100 item)


  let p_vznikne_kruh_vetsi_padesat = (0.5 + ([1..49] |> List.sumBy (fun i -> 1.0 / (100.0 + 1.0 - float i) * (50.0 - float i) / (100.0 - float i))))

  let p_preziji = 1.0 - p_vznikne_kruh_vetsi_padesat
  let check = 1.0 - ([51..100] |> List.sumBy (fun item -> 1.0 / float item))

  let aaa = (99.0 * 98.0 + 99.0 + 98.0 + 1.0)
  let bbb = (99.0 * 98.0 * 97.0 + 99.0 * 98.0 + 99.0 * 97.0 + 98.0 * 97.0 + 99.0 + 98.0 + 97.0 + 1.0) / 100.0 / 99.0 / 98.0


  let ch1 = 1.0 - ([51..100] |> List.sumBy (fun i -> 1.0 / float i))
  let ch2 = 1.0 - 0.5 - ([1..49] |> List.sumBy (fun i -> (50.0 - float i) / (101.0 - float i) / (100.0 - float i)))

  let ch1 = [51..100] |> List.map (fun i -> 1.0 / float i) |> List.sum
  let ch2 = 100.0 / 202.0 + ([0..49] |> List.map (fun i -> (50.0 - float i) / (101.0 - float i) / (100.0 - float i)) |> List.sum)

  let a = 1.0 / 4.0 + 1.0 / 5.0 + 1.0 / 6.0
  let b = 1.0 / 2.0 + 2.0 / 6.0 / 5.0 + 1.0 / 5.0 / 4.0

module SomeTest =
  #r @"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.0\System.Windows.Forms.dll"
  open System.Windows.Forms  

  type Wrapper(s:obj) = 
    member x.Value = s.ToString()

  let grid<'T> (x:seq<'T>) =     
    let form = new Form(Visible = true)    
    let data = new DataGridView(Dock = DockStyle.Fill)    
    form.Controls.Add(data)    
    data.AutoGenerateColumns <- true
    if typeof<'T>.IsPrimitive || typeof<'T> = typeof<string> then
      data.DataSource <- [| for v in x -> Wrapper(box v) |]
    else 
      data.DataSource <- x |> Seq.toArray

  let listOfTuples = List.init 10 (fun x -> (x, x*x))
  type MyRecord = { Name : string; Number : int }
  let listOfRecords = [{Name = "Alpha"; Number = 1}; {Name = "Bravo"; Number = 2}]
  
  grid [ 1 .. 10 ]
  grid [ "Hello"; "World" ]
  grid listOfTuples
  grid listOfRecords


// Compute all strings in Skla project.
// File new contains the result of "Find '"' in entire solution."
// I neet to exclude all .resx files, designer files etc.
let unwanted = [".resx"; ".Designer."; ".config"; "dbobject"; "///"; "param"; "cmd"; "Parameters"; "[\""; "\";\""; "\", \""]
let leaveOut (s : string) =
  unwanted |> List.exists (fun x -> s.Contains(x)) 
let path = @"c:\Users\Tomas\Desktop\new.txt"
let exists = System.IO.File.Exists(path);
let lines =
  path
  |> System.IO.File.ReadLines       // expose as seq<string>
  |> Seq.toList
  |> Seq.filter (fun x -> not (leaveOut x))
  |> Seq.toList
  |> List.map (fun x -> x.Replace(@"  C:\Users\Tomas\Documents\Drosera\Glass\Projekty\skla\srcwork", ""))
let count = lines |> List.length

// How much degrees correspon to atan 0.7?
let deg rad = 180.0 / System.Math.PI * rad
deg (System.Math.Atan(0.7))

// None value is represented as null and prints as null
// (printfn does not bother to call ToString() for null values)
// http://stackoverflow.com/questions/10435052/f-passing-none-to-function-getting-null-as-parameter-value
let testOptionPrinting () = 
  printfn "Some value is printed as: %A" (Some 1.0) // "Some value is printed as: Some 1.0"
  printfn "None value is printed as: %A" None       // "None value is printed as: <null>"

// Types can not be defined within a function
let myFunc x =
  // "error FS0010: Incomplete structured construct at or before this point in binding"
  //type myRecord = {x:float; y:float} 
  ()

let defineDictionary () =
  let dict = new System.Collections.Generic.Dictionary<_,_>(["add", (+); "sub", (-)] |> Map.ofList)
  let aaa = dict.["add"] 4 5
  do dict.Add("mult", (fun x y -> x * y))
  let bbb = dict.["mult"] 4 6
  ()

// Adding and multiplying function
// g(x) = f(x) + f(x) * f(x)
let add f g x =
  f x + g x
let mul f g x =
  f x * g x
let f x = x
let g = add f (mul f f)
let res = g 3.0

// Single case active pattern
let (|IsATron|) (s : string) = s.EndsWith("TRON")

// The test expression is passed as an input to the active pattern
// and its output value is matched to the pattern on the right of the active pattern's name.
let response =
   match "JessiTRON" with
    | IsATron true -> "Yeah! TRON!"
    | x -> "You should change your name to " + x + "iTRON"

// Formated printing
// Function sprintf prints to string object
let s1 = sprintf "%f, %g" 1.0 1.0
let s2 = sprintf "%f, %g" 1.1 1.1
let s3 = sprintf "%f, %g" 0.1 0.1

// Snippets from http://fsharpforfunandprofit.com/posts/defining-functions/
// Combinators uses only its parameters and no other function in its body. I.e. only function application.
module Combinators =
  let (|>) x f = f x // Forward pipe
  let (<|) f x = f x
  let (>>) f g x = g (f x)
  let (<<) g f x = g (f x)

  // Famous combinators whit names of birds
  let I x = x                // identity function, or the Idiot bird
  let K x y = x              // the Kestrel
  let M x = x >> x           // the Mockingbird
  let T x y = y x            // the Thrush (this looks familiar!)
  let Q x y z = y (x z)      // the Queer bird (also familiar!)
  let S x y z = x z (y z)    // The Starling
  // and the infamous...
  let rec Y f x = f (Y f) x  // Y-combinator, or Sage bird

module CurryDecurry =
  type CurriedFunc =
  | Curried1 of (int -> int)
  | Curried2 of (int -> int -> int)
  type TupledFunc =
  | Tupled1 of (int -> int)
  | Tupled2 of ((int * int) -> int)  

module Boxing = 
  let c = '7'
  let n = int c
  let cObj = box c
  let nObj = box n

module ListTest =
  type 'a SingleLinkedList =
    | Item of Head:'a * Tail:'a SingleLinkedList
    | Empty       
  let empty = Empty
  let oneItem = Item(7.0, Empty)
  let cons item list = Item(item, list)
  let oneItemV2 = cons 8.0 Empty
  let twoItems = cons 9.0 oneItemV2 
  let (( ^-> )) = cons
  let aaa = 1.0 ^-> 2.0 ^-> Empty

module LambdaCalculus =
  // Church numerals
  let num0 f x = x
  let num1 f x = f x
  let num2 f x = f (f x)
  let num3 f x = f (f (f x))
  
  // Basic operators
  let succ n f x = f (n f x)
  let plus m n = m succ n
  let mult m n = m (plus n) num0
  let pow b e = e b

  let res3 f x  = succ num2 f x
  let res4 f x = succ num3 f x 
  let res0 f x = plus num0 num0 f x 
  let res6 f x = mult num2 num3 f x 
  let res1 f x = pow num0 num0 f x 

  // Printing
  let f () = printf "I"
  let x = ()
  
  // Tests
  res3 f x 
  res4 f x
  res0 f x
  res6 f x
  res1 f x

module Casting  =
  let x = 1.0
  let o = box x // Converts to type obj
  let y = unbox<float> o
  let t1 = typeof<float>
  let t2 = typeof<obj>
  let l1 = [0..9]
  let iEnum = l1 :> System.Collections.Generic.IEnumerable<int>
  let l2 = iEnum :?> list<int>
  let l3 = iEnum :?> list<double> // Error

// How to define and implement interface
module Interfaces =
  type IPrintable =
    abstract member Print : unit -> unit

  type SomeClass1(x: int, y: float) =
    let sum = float x + y
    interface IPrintable with 
      member this.Print() = printfn "Class 1 with values %d %f" x y
    member this.Sum = sum

  type SomeClass2() =
    interface IPrintable with 
      member this.Print() = printfn "Just some class of 2nd type."

  let list = [SomeClass1(1, 1.0) :> IPrintable; SomeClass2() :> IPrintable]
  do list |> List.iter (fun x -> x.Print())
