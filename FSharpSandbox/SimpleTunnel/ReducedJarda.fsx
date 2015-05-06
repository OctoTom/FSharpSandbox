// Reference the MathNet.Numerics libraries
#r @"C:\Users\Tomas\OneDrive\Tech\Projects\FunMuMech\packages\MathNet.Numerics.3.2.3\lib\net40\MathNet.Numerics.dll"
#r @"C:\Users\Tomas\OneDrive\Tech\Projects\FunMuMech\packages\MathNet.Numerics.FSharp.3.2.3\lib\net40\MathNet.Numerics.FSharp.dll"

namespace SimpleTunnel

open MathNet.Numerics.LinearAlgebra.Double

module ReducedJarda =
  open System
  open System.Windows.Forms
  open MathNet.Numerics.LinearAlgebra.Double
  open MathNet.Numerics.LinearAlgebra.Generic
  open FSharp.Charting

  open LogModule
  open OptimModule

  let E = 15000.0
  let nu = 0.35
  let gamma = 18.0
  let H1 = 20.0
  let H2 = 8.0
  let H3 = 12.0
  let R = 3.5
  let dz = 1.5
  let alpha_z = 0.1
  let alpha_x = 0.5152510053
  let B = 6.353570591
  let D = 0.5152510053
  let e = 2.71828

  let E_oed = (E*(1.0-nu))/((1.0+nu)*(1.0-2.0*nu));
  let G = E/(2.0*(1.0+nu));

  let pow = System.Math.Pow

  let indexIsCorrect i = i >= 0 && i <= 4 
  let K (i, j) =
    match (i, j) with
    | (i, j) when not (indexIsCorrect i && indexIsCorrect j) -> failwith "Wrond indexes %A and %A" i j
    | (0, 0)          -> E_oed*(1.0/H1)*(2.0*B+(1.0/alpha_x))*(dz+(1.0/alpha_z))+G*(H1/3.0)*(alpha_x*dz+2.0*B*alpha_z+alpha_x/alpha_z+alpha_z/alpha_x)
    | (0, 1) | (1, 0) -> -E_oed*(1.0/H1)*(2.0*D+(1.0/alpha_x))*(dz+(1.0/alpha_z))*(2.0-pow(e, -alpha_x*(B-D)))+G*(H1/6.0)*(alpha_z*(2.0*D+2.0*(B-D)/3.0)+(2.0/(B-D))*(dz+1.0/alpha_z))
    | (0, 2) | (2, 0) -> -G*(2.0/6.0)*H1*alpha_z*(2.0*D+B-D)
    | (1, 1)          -> E_oed*((1.0/H1)+(1.0/H2))*(2.0*D+(1.0/alpha_x))*(dz+(1.0/alpha_z))+G*((H1/3.0)+(H2/3.0))*(alpha_x*dz+2.0*D*alpha_z+alpha_x/alpha_z+alpha_z/alpha_x)
    | (1, 2) | (2, 1) -> -G*(2.0/3.0)*H1*((1.0/(B-D))*(1.0-pow(e,-alpha_x*(B-D)))*(1.0+(1.0/2.0*alpha_z)-(alpha_z/pow(alpha_x,2.0)))+2.0*D*alpha_z/2.0)
    | (1, 3) | (3, 1) -> -E_oed*(1.0/H2)*(2.0*D+(1.0/alpha_x))*(dz+(1.0/alpha_z))+G*(H2/6.0)*(alpha_x*dz+2.0*D*alpha_z+alpha_x/alpha_z+alpha_z/alpha_x)
    | (2, 2)          -> E_oed*(16.0/3.0)*(1.0/H1)*(2.0*D*(B-D))*(dz+(1.0/alpha_z))+G*(16.0/30.0)*H1*(alpha_z*(2.0*D+2.0*(B-D)/3.0)+(2.0/(B-D))*(dz+1.0/alpha_z))
    | (3, 3)          -> E_oed*((1.0/H2)+(1.0/H3))*(2.0*D+(1.0/alpha_x))*(dz+(1.0/alpha_z))+G*((H2/3.0)+(H3/3.0))*(alpha_x*dz+2.0*D*alpha_z+alpha_x/alpha_z+alpha_z/alpha_x)
    | (3, 4) | (4, 3) -> -G*(H3/3.0)*(alpha_x*dz+2.0*D*alpha_z+(alpha_x/alpha_z)+(alpha_z/alpha_x))
    | (4, 4)          -> E_oed*(16.0/3.0)*(1.0/H3)*(2.0*D+(1.0/alpha_x))*(dz+(1.0/alpha_z))+G*(16.0/30.0)*H3*(alpha_x*dz+2.0*D*alpha_z+(alpha_x/alpha_z)+(alpha_z/alpha_x))
    | _ -> 0.0

  //let Kmat = DenseMatrix.Create(5, 5, K) :> Matrix

//  let Kmat_inv = Kmat.Inverse()
//
//  let test = Kmat * Kmat_inv

