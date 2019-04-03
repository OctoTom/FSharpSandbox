#load @"c:\Users\Tomas\Scripts\Paket\References.fsx"
open FSharp.Charting

/// Eurocode spectrum in SDOF period domain.
/// Horizontal elastic response spectrum
/// Defined in Eurocode 8: Design of structures for earthquake resistance, section 3.2.2.2.
module EurocodeSpectrum =
  /// Returns spectrum in form of a function of period T [s].
  let getHorizontalResponseSpectrum (tb, tc, td, eta, ag, s) =
    let func t =
      match t with
      | t when t < 0.0 -> failwith "Negative period time T."
      | t when t < tb -> ag * s * (1.0 + t / tb * (2.5 * eta - 1.0))
      | t when t < tc -> 2.5 * eta * ag * s
      | t when t < td -> 2.5 * eta * ag * s * tc / t
      | t when t < 4.0 -> 2.5 * eta * ag * s * tc * td / t**2.0
      | _ -> 0.0 // For periods longer than 4 seconds.
    func

module EurocodeSpectrum_TEST =
  // These values are for Type A ground
  let s = EurocodeSpectrum.getHorizontalResponseSpectrum (0.15, 0.4, 2.0, 1.0, 1.0, 1.0)
  let times = [0.0..0.01..5.0]
  times |> CommonTools.List.mapio s |> Chart.Line |> Chart.Show

