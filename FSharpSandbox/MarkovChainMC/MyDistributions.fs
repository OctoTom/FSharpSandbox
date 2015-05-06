module MyDistributions 

  // Open the modules
  open System
  open FSharp.Charting
  open MathNet.Numerics
  open MathNet.Numerics.Random
  open MathNet.Numerics.Statistics
  open MathNet.Numerics.Distributions
  open MathNet.Numerics.Statistics.Mcmc
  open MatrixModule

  // Normal distribution
  let normalDistribution = new MathNet.Numerics.Distributions.Normal()
  let normalPDF x = normalDistribution.Density(x)
  let normalCDF x = normalDistribution.CumulativeDistribution(x)
  let normalCDFinv p = normalDistribution.InverseCumulativeDistribution(p)

  let normalPDF2D (covMat: Matrix) (mu: float * float) x1 x2 =
    do if (covMat.[0,1] <> covMat.[1,0]) then raise (new Exception("Covariance matrix is not symmetric."))
    let mu_x1 = fst mu
    let mu_x2 = snd mu
    let sig_x1 = sqrt(covMat.[0,0])
    let sig_x2 = sqrt(covMat.[1,1])
    let rho = covMat.[0,1] / (sig_x1 * sig_x2)
    let multiplicator = 1.0 / 2.0 / Math.PI / sig_x1 / sig_x2 / sqrt(1.0 - rho * rho)
    multiplicator * exp(-1.0 / 2.0 / (1.0 - rho * rho) * ((x1 - mu_x1) * (x1 - mu_x1) / sig_x1 / sig_x1 + (x2 - mu_x2) * (x2 - mu_x2) / sig_x2 / sig_x2 - 2.0 * rho * (x1 - mu_x1) * (x2 - mu_x2) / sig_x1 / sig_x2))

  // Sample generators //
  /// .NET Random number generator
  let random = new System.Random()
  /// Infinite sequence of uniformly distributed random numbers from interval (0.0, 1.0).
  let samplesUD = Seq.unfold (fun () -> Some(random.NextDouble(), ())) ()
  /// Infinite sequence of 1D samples of with inverse distribution cdfInv.
  let sample cdfInv = samplesUD |> Seq.map cdfInv 
  /// Infinite seqvence of 1D samples with standard normal distribution.
  let samplesND = sample normalCDFinv
  /// Sequence of non-correlated 2D samples (u1, u2) with standard normal distribution.
  let normDistPoints = Seq.zip samplesND samplesND
  /// Infinite sequence of samples of two correlated normally distributed random variables X1 and X2.
  /// The joint PDF is described by 2x2 covariance matrix cov and pair of mean values mu.
  let normDistPointsCor (cov: Matrix) (mu: float * float) =
    let A = cov.CholeskyDecomp2x2
    let getCorrSample (u1, u2) =
      let x1 = A.[0,0] * u1 + A.[0,1] * u2 + fst(mu)
      let x2 = A.[1,0] * u1 + A.[1,1] * u2 + snd(mu)
      (x1, x2)
    normDistPoints |> Seq.map getCorrSample

  // Print outs
  let res1 = samplesUD |> Seq.take 4 |> Seq.toList
  let res2 = samplesND |> Seq.take 100 |> Seq.toList

  Chart.Combine [
    [-3.0..0.2..3.0] |> List.map (fun x -> (x, normalPDF x)) |> Chart.Line
    [-3.0..0.2..3.0] |> List.map (fun x -> (x, normalCDF x)) |> Chart.Line
    res2 |> List.map (fun x -> (x, 0)) |> Chart.Point
  ] |> ignore

  let numSamples = 1000
  let domainSize = 3.0
  let domainCorners = [(1.0, 1.0); (1.0, -1.0); (-1.0, -1.0); (-1.0, 1.0)] |> List.map (fun (x, y) -> (domainSize * x, domainSize * y))
  Chart.Combine [
    domainCorners |> Chart.Point
    normDistPoints |> Seq.take numSamples |> Chart.Point
  ] |> ignore

  // Test of samples form X1 and X2 with joint PDF. These samples has prescribed covariation, variance and mean value.
  let covMat = Matrix(array2D[[1.0; 0.5]; [0.5; 1.0]])
  let mu = (0.0, 0.0)
  Chart.Combine [
    normDistPointsCor covMat mu |> Seq.take numSamples |> Chart.Point
    [mu] |> Chart.Point
  ] |> ignore

