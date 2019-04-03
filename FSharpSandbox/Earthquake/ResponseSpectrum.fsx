#load @"c:\Users\Tomas\Scripts\Paket\References.fsx"
open FSharp.Charting

/// Forward interation of SDOF used to calculate the Response spectrum.
/// Described in the paper by Tom Irvine (stored in Mendeley)
/// omega_n ... natural angular frequency
/// xi ... damping ratio
/// Q ... amplification factor, Q = 1 / (2 xi), (Tom Irvine)
/// x ... displacement of mass m
/// y ... prescribed displacement of base
module TIrvine =
    let exp x = System.Math.Exp(x)
    /// Returns natural frequency of a damped oscilator
    let getOmega_n k m = sqrt(k / m)
    /// Returns damping ratio
    let getXi c m omega_n = c / m / 2.0 / omega_n
    /// Returns damped angular frequency
    let getOmega_d omega_n xi = omega_n * sqrt(1.0 - xi * xi)
    let ddx_new xi omega_n omega_d dt ddx_i1 ddx_i2 ddy_i ddy_i1 =
        2.0 * exp(-xi * omega_n * dt) * cos(omega_d * dt) * ddx_i1
        - exp(-2.0 * xi * omega_n * dt) *  ddx_i2
        + 2.0 * xi * omega_n * dt * ddy_i
        + omega_n * dt * exp(-xi * omega_n * dt) * ((omega_n / omega_d * (1.0 - 2.0 * xi * xi)) * sin(omega_d * dt) - 2.0 * xi * cos(omega_d * dt)) * ddy_i1
    /// Returns response of the SDOF to the prescribed history of base accelerations.
    /// omega_n ... natrural angular frequency of the SDOF
    /// xi ... damping
    /// dt ... time step
    /// ys ... history of base accelerations
    let getSDOFResponse omega_n xi dt (ys : float list) =
        // Damped natural angular frequency
        let omega_d = getOmega_d omega_n xi
        // Initialize the array...
        let xsArray = Array.init ys.Length (fun _ -> 0.0)
        // ...fill it... 
        for i in [1 .. ys.Length - 1] do
            let ddx_i1 = xsArray.[i - 1]
            let ddx_i2 = if i = 1 then 0.0 else xsArray.[i - 2]
            let ddy_i = ys.[i]
            let ddy_i1 = ys.[i - 1]
            xsArray.[i] <- ddx_new xi omega_n omega_d dt ddx_i1 ddx_i2 ddy_i ddy_i1
        // ...and return it.
        xsArray |> Array.toList
    /// Returns minimum, maximum and max(abs(min), abs(max)) of the response signal.
    let getSDOFResponseMinMaxAbs omega_n xi dt ys =
        let xs = getSDOFResponse omega_n xi dt ys
        let (min, max) = xs |> List.min, xs |> List.max
        min, max, System.Math.Max(abs(min), abs(max))
    /// Returns data (points) of SDOF response spectrum for a given base accelerations
    /// ys ... history of base accelerations
    /// dt ... time step
    /// xi ... damping
    /// fns ... natrural frequencies of the SDOFs
    let getSDOFResponseSpectrum ys dt xi omegas_n =
        omegas_n |> List.map (fun omgn -> getSDOFResponseMinMaxAbs omgn xi dt ys |> CommonTools.Misc.third)

//
TIrvine.getSDOFResponse 1.1 0.05 0.1 [1.0; 2.0; -2.0; -10.0]
TIrvine.getSDOFResponseMinMaxAbs 1.1 0.05 0.1 [1.0; 2.0; -2.0; -10.0]
TIrvine.getSDOFResponseSpectrum [1.0; 2.0; -2.0; -10.0] 0.1 0.05 [0.001; 0.01; 0.1; 1.0; 10.0; 100.0; 1000.0; 10000.0]

/// Benchmark in Tom Irvine's paper
module TIrvineHalfSineExample =
    let halfSine t =
        let duration = 0.011 // 11ms
        if t >= 0.0 && t <= duration then
            50.0 * sin (System.Math.PI / duration * t)
        else
            0.0
    let drawSDOFResponseToHalfSineBaseInput fn Q dt duration =
        let omega_n = 2.0 * System.Math.PI * fn
        let xi = 1.0 / 2.0 / Q
        // List of times
        let ts = [0.0 .. dt .. 0.06]
        let ys = ts |> List.map halfSine
        let xs = TIrvine.getSDOFResponse omega_n xi dt ys
        [
        (ts, ys) ||> List.zip |> FSharp.Charting.Chart.Line
        (ts, xs) ||> List.zip |> FSharp.Charting.Chart.Line
        ] |> FSharp.Charting.Chart.Combine |> FSharp.Charting.Chart.Show
    let getSDOFResponseToHalfSineBaseInputAbs fn Q dt duration =
        let omega_n = 2.0 * System.Math.PI * fn
        let xi = 1.0 / 2.0 / Q
        // List of times
        let ts = [0.0 .. dt .. 0.06]
        let ys = ts |> List.map halfSine
        let _, _, absValue = TIrvine.getSDOFResponseMinMaxAbs omega_n xi dt ys
        absValue
    // TESTS in the paper
    let drawAll () =
        drawSDOFResponseToHalfSineBaseInput 30.0 10.0 0.0001 0.6
        drawSDOFResponseToHalfSineBaseInput 80.0 10.0 0.0001 0.6
        drawSDOFResponseToHalfSineBaseInput 140.0 10.0 0.0001 0.6
    let displayMaxResponse () =
        printfn "Maximum response to half-sine impuls:"
        printfn "fn = %A, acc = %A" 30.0 (getSDOFResponseToHalfSineBaseInputAbs 30.0 10.0 0.0001 0.6)
        printfn "fn = %A, acc = %A" 80.0 (getSDOFResponseToHalfSineBaseInputAbs 80.0 10.0 0.0001 0.6)
        printfn "fn = %A, acc = %A" 140.0 (getSDOFResponseToHalfSineBaseInputAbs 140.0 10.0 0.0001 0.6)
    let drawSDOFResponseSpectrumHalfSine () =
        let Q = 10.0
        let dt = 0.0001
        let duration = 0.06
        let xi = 1.0 / 2.0 / Q
        let ts = [0.0 .. dt .. duration]
        let ys = ts |> List.map halfSine
        let fns = [5.0 .. 5.0 .. 1000.0]
        let omegas_n = fns |> List.map (fun x -> 2.0 * System.Math.PI * x) 
        let maxXs = TIrvine.getSDOFResponseSpectrum ys dt xi omegas_n
        FSharp.Charting.Chart.Line((fns, maxXs) ||> List.zip).WithXAxis(Log=true, Min=5.0).WithYAxis(Log=true, Min=10.0) |> FSharp.Charting.Chart.Show
        

// Run the tests.
// The charts should be identical to what is in Tom Irvine's paper.
(*
TIrvineHalfSineExample.drawAll () // Displays three plots.
TIrvineHalfSineExample.displayMaxResponse () // Writes to console.
TIrvineHalfSineExample.drawSDOFResponseSpectrumHalfSine () // Display spectrum plot.
*)



    
        

  
