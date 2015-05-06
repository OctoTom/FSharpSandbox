module Histogram

  // Converts list of items to list of pairs of neighbouring items in the input list
  let rec getPairs list =
    match list with
    | first::second::tail -> (first, second) :: getPairs (second::tail)
    | _ -> []
  // Computes midpoints of given bin
  let getBinMidpoints bins =
    bins |> getPairs |> List.map (fun (a, b) -> (a+b)/2.0) 
  
  // Computes frequencies of data withing given bin
  let getFrequencies bins data =
    // Indicates if given item falls in the given bin
    let fallsInBin bin item =
      fst bin <= item && item < snd bin
    // Computes number of items from a given list that fall in a given bin.
    let getCount list bin =
      let falls = fallsInBin bin
      list |> List.filter falls |> List.length
    bins |> getPairs |> List.map (getCount data)
  
  // Computes cumulative frequencies. First value is the height of first histogram. Last value is total number of samples.
  let getCumulativeFrequencies frequencies =
    let aux = frequencies |> List.fold (fun state item -> (List.head state + item)::state) [0.0]
    // Revert and remove the initial zero
    aux |> List.rev |> List.tail

  // Normalizes the list of frequencies to sum to 1.0.
  let normalize frequencies =
    let sum = frequencies |> List.sum |> float
    frequencies |> List.map (fun x -> float x / sum)
  
  /// <summary>Gives the points of CDF based on given frequencies</summary>
  /// <param name="bins">List of tuples containing left and right endpoint of bin.</param>
  /// <param name="ncf">Normalized cumulative frequencies.</param>
  let observedCDF bins ncf =
    // Frequencies starting with zero
    let ncf = 0.0 :: ncf // Shadowning original frequencies
    let xs = (bins |> List.head |> fst) :: (bins |> List.map snd)
    List.zip xs ncf

  /// <summary> Represents the inverse CDF </summary>
  let inverseCDF points probability =
    // Check of probability range
    if probability < 0.0 || probability > 1.0 then failwith ("Probability is out of the interval [0.0, 1.0].")
    // Pairs of points
    let intervals = getPairs points
    // Indicates if a given porbability fits to given interval
    let fitsToInterval interval =
      let firstProbab = interval |> fst |> snd
      let secondProbab = interval |> snd |> snd
      firstProbab <= probability && probability < secondProbab
    // Finds the interval in which the probability fits
    let findInterval = List.find fitsToInterval
    //
    let approximateOnInterval interval =
      let firstProbab = interval |> fst |> snd
      let secondProbab = interval |> snd |> snd
      let factor = (probability - firstProbab) / (secondProbab - firstProbab)
      let firstX = interval |> fst |> fst
      let secondX = interval |> snd |> fst
      firstX + factor * (secondX - firstX)
    intervals |> findInterval |> approximateOnInterval


