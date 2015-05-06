namespace ToolsLibrary

module Random =
  // The fucntional way to represent random number is IMHO to create a infinite sequence of random numbers of given type in given range.
  // This approach would hide the side effect of Random.Next() method.

  // Types of random integers we can generate.
  type RandomIntType =
    | RandomInt
    | RandomIntTo of int
    | RandomIntFromTo of int * int
  
  // Function that returns infinite sequence of random ints.
  let randomIntSequence randomIntType =
    let random = System.Random()
    let generator =
      match randomIntType with
      | RandomInt ->
        printfn "You want some rnd int."
        fun () -> random.Next()
      | RandomIntTo(maxValue) ->
        printfn "You want some rnd int up to %d." maxValue
        fun () -> random.Next(maxValue)
      | RandomIntFromTo(minValue, maxValue) ->
        printfn "You want some rnd int from %d up to %d." minValue maxValue
        fun () -> random.Next(minValue, maxValue)
    Seq.initInfinite (fun index -> generator())

  // Usage
  let res1 = randomIntSequence RandomInt |> Seq.take 20 |> Seq.toList
  let res2 = randomIntSequence (RandomIntTo(10)) |> Seq.take 20 |> Seq.toList
  let res3 = randomIntSequence (RandomIntFromTo(10, 15)) |> Seq.take 20 |> Seq.toList

  // Types of random floats we can generate.
  type RandomFloatType =
  | RandomFloat
  | RandomFloatTo of float
  | RandomFloatFromTo of float * float

  // Function that returns infinite sequence of random floats.
  let randomFloatSequence randomFloatType =
    let random = System.Random()
    let generator = random.NextDouble
    match randomFloatType with
    | RandomFloat ->
      Seq.initInfinite (fun index -> generator())
    | RandomFloatTo(maxValue) ->
      Seq.initInfinite (fun index -> generator() * maxValue)
    | RandomFloatFromTo(minValue, maxValue) ->
      Seq.initInfinite (fun index -> minValue + generator() * (maxValue-minValue))
  
  // Usage
  let res4 = randomFloatSequence RandomFloat |> Seq.take 20 |> Seq.toList
  let res5 = randomFloatSequence (RandomFloatTo(10.0)) |> Seq.take 20 |> Seq.toList
  let res6 = randomFloatSequence (RandomFloatFromTo(10.0, 15.0)) |> Seq.take 20 |> Seq.toList

