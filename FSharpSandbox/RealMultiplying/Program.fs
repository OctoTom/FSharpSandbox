/// My experimental way of approximate real numbers multiplying.
/// The numbers are equidistant on log-scale axis of positive real numbers.
/// Multiplying these particular numbers reduces to "walking up or down" this geometric series.
/// Multiply by one means to stay at the current number.
/// Multiply by 1.6 means to move two steps up, etc.
module RealMultiplying =
  let numBase = [1.0; 1.25; 1.6; 2.0; 2.5; 3.2; 4.0; 5.0; 6.3; 8.0]
  let rec rankToReal a =
    match a with
    | a when a < 0 -> rankToReal (a%10+10) * (10.0)**(float (a / 10) - 1.0)
    | a when a < 10 -> numBase.[a]
    | _ -> rankToReal (a%10) * (10.0)**(float (a / 10))
  let rec realToRank a =
    let order = System.Math.Floor (System.Math.Log10 a)
    let real = a / 10.**order
    let index = numBase |> List.findIndex (fun item -> System.Math.Abs(item-real) < 0.01) // Compare with tolerance.
    index + 10 * int order
  let mult a b = rankToReal(realToRank a + realToRank b)
  let rnd = System.Random()
  let rndSeq = Seq.initInfinite (fun idx -> rnd.Next(0, 10) |> rankToReal)
  let repl sqn =
    System.Console.WriteLine("Hi! This is Real-Multiplying excersise.")
    System.Console.WriteLine("Press Enter repeatedly to get numbers to multiply and the result.")
    System.Console.WriteLine("Press write something and press Enter to exit.")
    let rec repl sqn  =
      match sqn, System.Console.ReadLine() with
      | a::b::t, "" ->
        let a = 2.5 // Shadow the value if you want to prescribe the left operand.
        System.Console.Write("Calculate " + string a + " * " + string b)
        System.Console.ReadLine() |> ignore
        System.Console.Write("Result = " + string (mult a b))
        repl t
      | _ -> ()
    repl sqn
  do rndSeq |> Seq.take 1000 |> Seq.toList |> repl 

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
