  // Vrhy minci
  let tosses = [0; 1; 1; 1; 1; 0; 1; 1; 1; 0; 1; 1; 0; 0; 0; 1]
  //let tosses = [for i in 1..100 -> 0] @ [for i in 1..20 -> 1]

  // Mince je rizena parametrem p = P(x=1). Protoze hodim jen 0 nebo 1 plati P(x=0) = 1-p
  
  // Likelihood
  let L theta x =
    match x with
    | 0 -> 1.0 - theta
    | 1 -> theta
    | _ -> failwith "Value of x is out of range."

  let likelihoods theta = tosses |> List.map (L theta)

  let density theta = likelihoods theta |> List.fold (*) 1.0 

  let points = [0..100] |> List.map (fun x -> 0.01 * float x)

  let relativeDensities = points |> List.map (fun x -> x, density x)

  let maxDensity = relativeDensities |> List.map snd |> List.max

  let normRelativeDensities = relativeDensities |> List.map (fun x -> fst x, snd x / maxDensity)

  normRelativeDensities |> FSharp.Charting.Chart.Line
