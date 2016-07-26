#r @"c:\Users\Tomas\Sync\Timber\LepenyNosnik\packages\MathNet.Numerics.3.11.0\lib\net40\MathNet.Numerics.dll"
#r @"c:\Users\Tomas\Sync\Timber\LepenyNosnik\packages\MathNet.Numerics.FSharp.3.11.0\lib\net40\MathNet.Numerics.FSharp.dll"
#r @"c:\Users\Tomas\OneDrive\OneSync\Projects\CommonTools\CommonTools\Bin\Release\CommonTools.dll"
#load @"c:\Users\Tomas\Scripts\Paket\LoadPackages.fsx"
open FSharp.Charting

/// Type of modulus. Storage or loss.
type ModulusType = Storage | Loss
let modulusTypeToString mt = match mt with Storage -> "S" | Loss -> "L"
let modulusTypeOfString s = match s with "S" -> Storage | "L" -> Loss | _ -> failwith "Unknown string representation of modulus type."
/// Record type for measured data point
type MeasuredData = { omega : float; T : float; G : float; modulType : ModulusType }

/// Horizontal shift factor
//let a (Tr:float) c1 c2 T = exp(-c1 * (T - Tr) / (c2 + T - Tr)) // This was wrong. In the paper is "log" which is common logarithm, not natural logarithm!
let a (Tr:float) c1 c2 T = 10.0**(-c1 * (T - Tr) / (c2 + T - Tr)) 
/// Storage modulus
let Gs Ginf GCell tauCell a omega =
  Ginf + (List.map2 (fun G tau -> tau * tau * a * a * omega * omega / (1.0 + tau * tau * a * a * omega * omega) * G) GCell tauCell |> List.sum)
/// Loss modulus
let Gl GCell tauCell a omega =
  List.map2 (fun G tau -> tau * a * omega / (1.0 + tau * tau * a * a * omega * omega) * G) GCell tauCell |> List.sum
/// Storage or loss modulus 
let Gsl modulusType Ginf GCell tauCell a omega =
  match modulusType with
  | Storage -> Gs Ginf GCell tauCell a omega
  | Loss -> Gl GCell tauCell a omega



// BENCHMARK DATA //
// Maxwell cells
/// All generated 
let tauCell = [1.0; 100.0; 10000.0]
let benchmarkData =
  let GCell = [1.5e5; 1.2e5; 1.0e5]
  let Ginf = 5.0e4
  // Shift factors parameters
  let c1 = 12.0
  let c2 = 80.0
  // Reference temperature
  let Tr = 30.0
  // Generate data for these values
  let omegaMeas = [1.0e-4; 1.0e-2; 1.0; 1.0e2]
  // Temperatures
  let TMeas = [30.0; 50.0; 80.0]

  /// Horizontal shift factor
  let a T = a Tr c1 c2 T
  /// Storage modulus. Shadowing
  let Gs (omega, T) = Gs Ginf GCell tauCell (a T) omega
  /// Loss modulus. Shadowing
  let Gl (omega, T) = Gl GCell tauCell (a T) omega

  /// Cartesian product of prescribed frequencies and temperatures.
  let pairs = CommonTools.List.cartesian omegaMeas TMeas

  let dataS = pairs |> List.map (fun pair -> {omega = fst pair; T = snd pair; G = Gs pair; modulType = Storage}) 
  let dataL = pairs |> List.map (fun pair -> {omega = fst pair; T = snd pair; G = Gl pair; modulType = Loss}) 
  // Return
  dataS @ dataL

// Print data
let printMeasuredData data =
  let toLine r = 
    printfn "%s, %f, %f, %f" (r.modulType |> modulusTypeToString) r.T r.omega r.G 
  data |> List.sortBy (fun r -> (r.modulType, r.T, r.omega)) |> List.iter toLine
// Run it.
benchmarkData |> printMeasuredData


//////////////////////////////////////////////////////////////////////////////
// Measured storeage and loss moduli from the figured in Andreozzi's paper. //
//////////////////////////////////////////////////////////////////////////////

// Read measured data for different temperatures and omegas from file.
let measuredData_Andreozzi =
  let header :: rows = CommonTools.IO.readLines @"c:\Users\Tomas\Sync\Glass\Data from Andreozzi paper - PVB_0.76mm.txt" |> Seq.toList
  let mapping (s : string) =
    let elements = s.Split([" "; "\t"] |> List.toArray, System.StringSplitOptions.RemoveEmptyEntries) |> List.ofArray
    let mt = modulusTypeOfString elements.Head
    let values = elements.Tail |> List.map float
    {omega = values.[1]; T = values.[0]; G = values.[2]*1.0e6; modulType = mt}
  rows |> Seq.toList |> List.map mapping


// Cell data from Andreozzi's paper
// PVB@0.76
let Gi = [514628.0; 280116.0; 144282.0; 86904.0; 76190.0; 92202.0; 98780.0; 85555.0; 70251.0; 107653.0] // [Pa]
let ti = [9.51E-02; 4.71E-01; 2.72E+00; 2.11E+01; 2.21E+02; 2.12E+03; 1.74E+04; 1.31E+05; 1.05E+06; 2.99E+07] // [s]
// PVB@1.52
let Gi_doublePVB = [1066280.0; 560438.0; 279509.0; 135178.0; 87014.0; 104767.0; 134158.0; 135411.0; 111062.0; 125984.0] // [Pa]
let ti_doublePVB = [1.00E-01; 5.89E-01; 4.32E+00; 4.00E+01; 5.31E+02; 7.25E+03; 7.56E+04; 7.38E+05; 1.10E+07; 1.00E+08] // [s]

let Tref_Andreozzi = 30.0
let c1_Andreozzi = 12.5
let c2_Andreozzi = 89.0

// Compose master curve from the Andreozzi's data with their c1 and c2
let dataMasterCurve_Andreozzi =
  let getOmega T omega =
    a Tref_Andreozzi c1_Andreozzi c2_Andreozzi T * omega
  let mapping (r : MeasuredData) = {r with T = Tref_Andreozzi; omega = getOmega r.T r.omega}
  measuredData_Andreozzi |> List.map mapping
let dataStorage = dataMasterCurve_Andreozzi |> List.filter (fun r -> r.modulType = Storage) |> List.map (fun r -> r.omega, r.G) |> Chart.Point
let dataLoss = dataMasterCurve_Andreozzi |> List.filter (fun r -> r.modulType = Loss) |> List.map (fun r -> r.omega, r.G) |> Chart.Point
let showMaterCurve_Andreozzi =
  Chart.Combine([dataStorage; dataLoss]).WithXAxis(Log=true).WithYAxis(Log=true)

// Compute master curve from the Andreozzi's Maxwell chain
let computedDataStorage, computedDataLoss =
  let tauCell = ti
  let GCell = Gi
  let Ginf = 0.0
  // Shift factors parameters
  let c1 = c1_Andreozzi
  let c2 = c2_Andreozzi
  // Reference temperature
  let Tr = 30.0
  /// Horizontal shift factor
  let a T = a Tr c1 c2 T
  /// Storage modulus. Shadowing
  let Gs (omega, T) = Gs Ginf GCell tauCell (a T) omega
  /// Loss modulus. Shadowing
  let Gl (omega, T) = Gl GCell tauCell (a T) omega
  let omegas = [-8.0..0.25..2.0] |> List.map (fun i -> 10.0**i)
  let plotPataStorage = omegas |> List.map (fun omega -> omega, Gs (omega, Tr)) |> Chart.Line
  let plotPataLoss = omegas |> List.map (fun omega -> omega, Gl (omega, Tr)) |> Chart.Line
  plotPataStorage, plotPataLoss


Chart.Combine([dataStorage; computedDataStorage]).WithXAxis(Log=true).WithYAxis(Log=true, Min=1000.0)
Chart.Combine([dataLoss; computedDataLoss]).WithXAxis(Log=true).WithYAxis(Log=true, Min=1000.0)

// Data computed from the Maxwell chain in Anreozzis paper //
// Maxwell cells
/// All generated 
let benchmarkData_Andreozzi =
  let tauCell = ti
  let GCell = Gi
  let Ginf = 0.0
  // Shift factors parameters
  let c1 = c1_Andreozzi
  let c2 = c2_Andreozzi
  // Reference temperature
  let Tr = 30.0

  /// Horizontal shift factor
  let a T = a Tr c1 c2 T
  /// Storage modulus. Shadowing
  let Gs (omega, T) = Gs Ginf GCell tauCell (a T) omega
  /// Loss modulus. Shadowing
  let Gl (omega, T) = Gl GCell tauCell (a T) omega
  
  let mapping (r : MeasuredData) =
    let G =
      match r.modulType with
      | Storage -> Gs (r.omega, r.T)
      | Loss -> Gl (r.omega, r.T)
    r, G
  
  measuredData_Andreozzi |> List.map mapping

/// Compares the Andreozzi's data for different temperatures with the values computed using their Maxwell chain.
let compareG mt T =
  let measured =
    benchmarkData_Andreozzi |> List.filter (fun (r, G) -> r.modulType = mt && r.T = T)
    |> List.map (fun (r, G) -> r.omega, r.G) |> Chart.Line
  let computed =
    benchmarkData_Andreozzi |> List.filter (fun (r, G) -> r.modulType = mt && r.T = T)
    |> List.map (fun (r, G) -> r.omega , G * 1.0e-6) |> Chart.Line
  Chart.Combine([measured; computed]).WithXAxis(Log=true).WithYAxis(Log=true)
compareG Storage 60.0



//////////////////////////////////
// IDENTIFICATION OF PARAMETERS //
////////////////////////////////// 

/// All measured data stored as a list of records.
let allData = measuredData_Andreozzi
/// All measured data grouped by type and temperature.
let showData = 
  let groups = allData |> List.groupBy (fun item -> item.modulType, item.T)
  /// Plot line for each group and combine them into one chart.
  (groups |> List.map (fun (_, group) -> group |> List.map (fun r -> r.omega, r.G) |> Chart.Line) |> Chart.Combine).WithXAxis(Log=true).WithYAxis(Log=true)


/// Reference temperature
let Tr = 30.0

// Characteristic times in cells. Chosen values
//let tau = tauCell
let tau = ti

let nCell = List.length tau

/// Type to store unknowns
type Unknowns = {G : float list; G_inf : float; c1 : float; c2 : float}
/// Converts unknowns stored as record into vector
let asVector (r : Unknowns) = MathNet.Numerics.LinearAlgebra.Vector<float>.Build.DenseOfEnumerable(r.G @ [r.G_inf; r.c1; r.c2])
/// Converts unknowns stored as vector into record
let asRecord (v : MathNet.Numerics.LinearAlgebra.Vector<float>) =
  if v.Count <> nCell + 3 then failwith "Wrong length of vector."
  let G = List.init nCell (fun i -> v.[i])
  let G_inf = v.[nCell]
  let c1 = v.[nCell + 1]
  let c2 = v.[nCell + 2]
  {G = G; G_inf = G_inf; c1 = c1; c2 = c2}

/// Number of unknowns computed from the number of Maxwell cells and additional unknowns in record.
let nUnknowns =
  let dummyG = List.init nCell (fun _ -> 0.0)
  (asVector {G = dummyG; G_inf = 0.0; c1 = 0.0; c2 = 0.0}).Count

/// Evaluates residua for given unknowns stored in vector.
let r x = 
  let unk = asRecord x
  let a T = a Tr unk.c1 unk.c2 T
  let Gcomp (point : MeasuredData) = Gsl point.modulType unk.G_inf unk.G tau (a point.T) point.omega
  let residua = allData |> List.map (fun point -> Gcomp point - point.G)
  MathNet.Numerics.LinearAlgebra.Vector<float>.Build.DenseOfEnumerable(residua)

/// Jacobian matrix of residua. Computed analytically.
let J (x : MathNet.Numerics.LinearAlgebra.Vector<float>) =
  let unk = asRecord x
  let a T = a Tr unk.c1 unk.c2 T
  let Gcomp (point : MeasuredData) = Gsl point.modulType unk.G_inf unk.G tau (a point.T) point.omega
  let dGi_dGinf i = match allData.[i].modulType with Storage -> 1.0 | Loss -> 0.0
  let dGi_dGj i j =
    let point = allData.[i]
    let a = a point.T
    let omega = point.omega
    let tau = tau.[j]
    match point.modulType with
    | Storage -> tau * tau * a * a * omega * omega / (1.0 + tau * tau * a * a * omega * omega)
    | Loss -> tau * a * omega / (1.0 + tau * tau * a * a * omega * omega)
  let dGi_dai i = 
    let point = allData.[i]
    let a = a point.T
    let omega = point.omega
    let mappingS G tau = 2.0 * tau**2.0 * a * omega**2.0 / (1.0 + tau * tau * a * a * omega * omega)**2.0 * G
    let mappingL G tau = (tau * omega - tau**3.0 * a**2.0 * omega**3.0) / (1.0 + tau * tau * a * a * omega * omega)**2.0 * G
    match point.modulType with
    | Storage -> List.map2 mappingS unk.G tau |> List.sum
    | Loss -> List.map2 mappingL unk.G tau |> List.sum
  let dai_dc1 i =
    let T = allData.[i].T
    let c1, c2 = unk.c1, unk.c2
    -(T - Tr) / (c2 + T - Tr) * exp(-c1 * (T - Tr) / (c2 + T - Tr))
  let dai_dc2 i =
    let T = allData.[i].T
    let c1, c2 = unk.c1, unk.c2
    c1 * (T - Tr) / (c2 + T - Tr)**2.0 * exp(-c1 * (T - Tr) / (c2 + T - Tr))
  let nPoint = allData.Length
  let J i j =
    match j with
    | j when j = nCell -> dGi_dGinf i
    | j when j = nCell + 1 -> dGi_dai i * dai_dc1 i
    | j when j = nCell + 2 -> dGi_dai i * dai_dc2 i
    | j -> dGi_dGj i j 
  let lol = List.init nPoint (fun i -> List.init nUnknowns (fun j -> J i j))
  let matrix = MathNet.Numerics.LinearAlgebra.Matrix<float>.Build.DenseOfRows (lol |> List.toSeq |> Seq.map List.toSeq)
  matrix


/// Comparison of numerical and analytical Jacobian
//let x0 = {G = [12.0; 18.0; 21.0]; G_inf = 31.0; c1 = 21.0; c2 = 18.0} |> asVector
//let J0 = J x0
//let r0 = r x0
//
//let idx = 0
//let f (x : float list) =
//  let xVec = MathNet.Numerics.LinearAlgebra.Vector<float>.Build.DenseOfEnumerable(x)
//  let rVec = r xVec
//  rVec.[idx]
//
//CommonTools.Math.getGrad f 0.1 (x0 |> Seq.toList)
//J0.Row(idx) |> Seq.toList

let lambda = 1.0
let I = MathNet.Numerics.LinearAlgebra.Double.DenseMatrix.CreateIdentity(nUnknowns)

let x0 = {G = List.init nCell (fun _ -> 1.0e6); G_inf = 1.0; c1 = 1.0; c2 = 1.0} |> asVector

let update x =
  let J = J x
  let r = r x
  let newX = x - (J.Transpose() * J + lambda * I).Inverse() * J.Transpose() * r
  newX.Map(fun x -> if x < 0.1 then 0.1 else x)

let generator x =
  let newX = update x
  Some(newX, newX)
let sqn = Seq.unfold generator x0

sqn |> Seq.skip 100 |> Seq.head |> Seq.toList

