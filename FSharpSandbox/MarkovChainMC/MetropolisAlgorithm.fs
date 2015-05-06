module MetropolisAlgorithm 

  // Open the modules
  open System
  open FSharp.Charting
  open MathNet.Numerics
  open MathNet.Numerics.Random
  open MathNet.Numerics.Statistics
  open MathNet.Numerics.Distributions
  open MathNet.Numerics.Statistics.Mcmc
  open MatrixModule
  open Histogram
  open MyDistributions

  // Helper functions:
  // Converts 2D vector to pair (FSharp.Charting accepts points as tuples)
  let listToTuple (x:list<'T>) = (x.[0], x.[1])
  // Converts pair to list
  let tupleToList (x0, x1) = [x0; x1]
  // Converts value to tuple containing the value 
  let valueToPair x = (x, x)
  // Converts false to 0.0 and true to 1.0
  let boolToFloat value = if value then 1.0 else 0.0

  // Random generator
  let generator = System.Random()
  let randomNumberUD x0 x1 = x0 + (x1 - x0) * generator.NextDouble()

  // Rectangular domain definition //
  // Boundaries of hyperrectangle
  let fromValues = [0.0; 10.0] // (x0, y0) // Left endpoints
  let toValues = [10.0; 20.0] // (x1, y1) // Right endpoints
  // Area (volume) or rectangle (hypercube)
  let domainArea = List.fold2 (fun acc fromVal toVal -> acc * (toVal - fromVal)) 1.0 fromValues toValues
  // Returns coordinates of random point uniformly distributed within given hypercube.
  let randomPoint fromValues toValues = List.map2 randomNumberUD fromValues toValues

  // Target distribution
  let covMat = Matrix(array2D[[2.0; 1.0]; [1.0; 2.0]])
  let mu = (1.0, 2.0)
  let distrib (x1, x2) = normalPDF2D covMat mu x1 x2 
  let target (x: float list) =
    do if (x.Length <> 2) then raise (new Exception("Target distribution: unsupported number of dimensions."))
    x |> listToTuple |> distrib
  
  // Indicators - functions that return 1.0 when point falls in given area and 0.0 when it falls outside.
  // Rectangular domain indicators
  let indicatorBool_HypercubeDomain r center x = List.forall2 (fun center x -> center - r < x && x < center + r) center x
  let indicatorFloat_HypercubeDomain r center x = (indicatorBool_HypercubeDomain r center x) |> boolToFloat
  // Circle domain indicators
  let indicatorBool_HypersphereDomain radius (center : float list) (x : float list) =
    sqrt((x.[0] - center.[0]) * (x.[0] - center.[0]) + (x.[1] - center.[1]) * (x.[1] - center.[1])) < radius
  let indicatorFloat_HypersphereDomain radius (center : float list) (x : float list) =
    (indicatorBool_HypersphereDomain radius center x) |> boolToFloat

  // Definition of failure area 
  let c1 = [3.0; 0.0] // Center of failure area
  let r1 = 1.5 // "Radius" of the increased failure area
  let squareIndicator = indicatorFloat_HypercubeDomain r1 c1
  let circleIndicator = indicatorFloat_HypersphereDomain r1 c1



  /// Represents a basic Metropolis-Hastings algorithm.
  /// The algorithm generates samples withnin given domanin (defined by indicator) which are distributed according to the target distribution.
  type Metropolis (target, indicator, startPoint, proposalDistributionWidth) =
      // Random generator
      let generator = System.Random()
      // Uniform distribution
      let randomNumberUD x0 x1 = x0 + (x1 - x0) * generator.NextDouble()
      // Proposal distribution. Simulates candidate xi (Greek letter) under condition of x.
      // This function returns a random point xi in surroundings to given point x. The random distribution is centered to x. 
      let proposalPoint x =
        x |> List.map (fun x -> randomNumberUD (x - proposalDistributionWidth / 2.0) (x + proposalDistributionWidth / 2.0))  
      // Defines probability that the new x is replaced
      let replaceProbabilityFormula f xi x = // Should return min{1.0, q(xi)/q(x)}
        min 1.0 ((f xi) / (f x))
      let replaceProbability = replaceProbabilityFormula target 
      // Get next sample using Metropolis algorithm
      let getNextX_Metropolis x =
        let xi = proposalPoint x
        let replaceProbability = replaceProbability xi x // Shadowing
        let candidate = if (generator.NextDouble() < replaceProbability) then xi else x // With replace probability 1.0 returns xi.
        if (indicator candidate > 0.0) then candidate else x  
      // Infinite sequence of samples generated with Metropolis algorithm
      let samplesSeq = Seq.unfold (fun x -> Some(valueToPair (getNextX_Metropolis x))) startPoint
      /// Generates samples
      /// <param name="count">Number of samples to be generated</param>
      member x.getSamples count = samplesSeq |> Seq.take count |> Seq.toList

  let numOfSim = 10000
  let metropolisDomain = new Metropolis(target, (fun _ -> 1.0), mu |> tupleToList, 10.0)
  let metropolisSubdomain = new Metropolis(target, circleIndicator, c1, 1.0)
  let samplesMetropolisDomain = metropolisDomain.getSamples numOfSim
  let samplesMetropolisSubdomain = metropolisSubdomain.getSamples numOfSim

  // Frame in chart
  let x1_min = -6.0
  let x1_max = 6.0
  let x2_min = -6.0
  let x2_max = 6.0
  let pointsOfFrame = [(x1_min, x2_min); (x1_min, x2_max); (x1_max, x2_max); (x1_max, x2_min); (x1_min, x2_min)]
  let metropolisSamplingDomainData = samplesMetropolisDomain |> List.map listToTuple
  let metropolisSamplingSubdomainData = samplesMetropolisSubdomain |> List.map listToTuple
  let directSamplingData = normDistPointsCor covMat mu |> Seq.take numOfSim |> Seq.toList


  // Draw combined charts (only in F# interactive)
  FSharp.Charting.Chart.Combine([
                                  Chart.Line(pointsOfFrame, Name="Frame").WithXAxis(Min = x1_min, Max = x1_max).WithYAxis(Min = x2_min, Max = x2_max);
                                  Chart.Point(metropolisSamplingSubdomainData, Name="Metropolis Subdomain", MarkerSize=2, MarkerColor=System.Drawing.Color.Black).WithXAxis(Min = x1_min, Max = x1_max).WithYAxis(Min = x2_min, Max = x2_max);
                                  Chart.Point(metropolisSamplingDomainData, Name="Metropolis Domain", MarkerSize=2, MarkerColor=System.Drawing.Color.Blue).WithXAxis(Min = x1_min, Max = x1_max).WithYAxis(Min = x2_min, Max = x2_max);
                                  Chart.Point(directSamplingData, Name="Direct", MarkerSize=2, MarkerColor=System.Drawing.Color.Red).WithXAxis(Min = x1_min, Max = x1_max).WithYAxis(Min = x2_min, Max = x2_max)
                                ]).WithXAxis(Min = x1_min, Max = x1_max).WithYAxis(Min = x2_min, Max = x2_max)

  let data = samplesND |> Seq.take 100 |> Seq.toList
  let bins = [for i in -10 .. 10  -> float i /2.0]
  let binsAsPairs = getPairs bins
  let midpoints = getBinMidpoints bins
  let frequencies = Histogram.getFrequencies bins data 
  let histogram = List.zip midpoints frequencies
  let cumulativeFrequencies =  frequencies |> normalize |> getCumulativeFrequencies
  let cumulativeHistogram = List.zip midpoints cumulativeFrequencies
  let cumulativeCDFPoints = observedCDF binsAsPairs cumulativeFrequencies

  //Chart.Column(histogramData, Name="Histogram", MarkerColor=System.Drawing.Color.Red)
  Chart.Column(histogram)
  Chart.Column(cumulativeHistogram)
  Chart.Line(cumulativeCDFPoints)


