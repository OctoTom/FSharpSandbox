namespace SimpleTunnel


open System
open System.Windows.Forms
open MathNet.Numerics.LinearAlgebra.Double
open FSharp.Charting

open LogModule
open OptimModule

module Model2d3d =
  // ==== GENERAL UTILS ====
  let shearModulus E nu = E / 2.0 / (1.0 + nu)
  let edomModulus E nu = E * (1.0 - nu) / (1.0 + nu) / (1.0 - 2.0 * nu)
  let vecToList (x:Matrix) = if x.ColumnCount <> 1 then failwith "Not a coulumn vector" else x.Column(0).ToArray() |> Array.toList
  /// Computes the total displacement (setlement) in 2d3d as a sum of all incremental settlements deltaDisp.
  /// Wtot = deltaW * (1 + 2 * exp(alpha * deltaZ / 2)/(exp(alpha * deltaZ / 2) - 1)  
  let totalDisp alpha_z deltaZ incremDisp =
    let c = alpha_z * deltaZ
    incremDisp * (1.0 + 2.0 * exp(c / 2.0) / (exp c - 1.0))
  /// Computes the increment of displacement that would generate the given total displacement.
  let incremDisp alpha_z deltaZ totalDisp =
    let c = alpha_z * deltaZ
    totalDisp / (1.0 + 2.0 * exp(c / 2.0) / (exp c - 1.0))

  // ==== STIFFNESS MATRICES AND LOAD VECTORS
  /// Stiffness matrix of simple excavation model created by prof. Sejnoha and Vlada Srnec.
  /// This model uses some kind of trigonometric function to approximate the settlements on terrain which proved wrong.
  let stiffnessMatrix (E, nu, alpha) (h1, h2, h3, L, r) =
    let pi = System.Math.PI
    let sin = System.Math.Sin
    let G = E / 2.0 / (1.0 + nu)
    let mult1 = E * (1.0 - nu) / (1.0 + nu) / (1.0 - 2.0 * nu) / alpha
    let mult2 = G / 6.0 * alpha
    let term1 = (8.0 + 3.0 * pi) * L / pi
    let aux = pi * (L - r) / L
    let term2 = (3.0 * (L - r) + L / pi * (sin aux + 8.0 * sin (aux / 2.0)))
    // Matrix K_I
    let K1_11 = (mult1 / h1 + 2.0 * mult2 * h1) * term1 // Same as K1_22
    let K1_12 = (-mult1 / h1 + mult2 * h1) * term1 // Same as K1_21
    // Matrix K_II
    let K2_22 = (mult1 / h2 + 2.0 * mult2 * h2) * term2 // Same as K2_33
    let K2_23 = (-mult1 / h2 + mult2 * h2) * term2 // Same as K2_32
    // Matrix K_III
    let K3_33 = (mult1 / h3 + 2.0 * mult2 * h3) * term1 
    // Localize
    DenseMatrix.ofArray2(array2D [[K1_11; K1_12; 0.0]; [K1_12; K1_11 + K2_22; K2_23]; [0.0; K2_23; K2_22 + K3_33]]) 

  /// Creates a load vector 3x1 representing the excvation forces.
  let loadVector (gamma, h0, r, deltaZ) =
    let Pi = System.Math.PI
    let F = Pi / 2.0 * gamma * h0 * r * deltaZ
    DenseMatrix.ofArray2(array2D [[0.0]; [-F]; [F]]) 

  /// Stiffness matrix (5 x 5) of simple tunnel model updated with buble functions.
  let stiffnessMatrix_v2 (E0, G, alpha_x, alpha_z) (H1, H2, H3, B, D, dz) =
    let a, b = alpha_z, alpha_x // podle Vladova mailu
    let K = DenseMatrix.zeroCreate 5 5
    K.[0,0] <- (2.0*B*G*H1**2.0*a**2.0*b + G*H1**2.0*a**2.0 + G*dz*H1**2.0*a*b**2.0 + G*H1**2.0*b**2.0 + 6.0*B*E0*dz*a*b + 3.0*E0*dz*a + 6.0*B*E0*b + 3.0*E0)/(6.0*H1*a*b)
    K.[0,1] <- -(exp(-b*(B - D))*(6.0*E0 - G*H1**2.0*a**2.0 - G*H1**2.0*b**2.0 + 6.0*E0*a*dz + 12.0*B*E0*b*exp(b*(B - D)) - G*H1**2.0*a*b**2.0*dz - 2.0*B*G*H1**2.0*a**2.0*b*exp(b*(B - D)) + 12.0*B*E0*a*b*dz*exp(b*(B - D))))/(12.0*H1*a*b)
    K.[0,2] <- 0.0
    K.[0,3] <- (D*G*H1*a)/6.0
    K.[0,4] <- 0.0
    K.[1,0] <- -(exp(-b*(B - D))*(6.0*E0 - G*H1**2.0*a**2.0 - G*H1**2.0*b**2.0 + 6.0*E0*a*dz + 12.0*B*E0*b*exp(b*(B - D)) - G*H1**2.0*a*b**2.0*dz - 2.0*B*G*H1**2.0*a**2.0*b*exp(b*(B - D)) + 12.0*B*E0*a*b*dz*exp(b*(B - D))))/(12.0*H1*a*b)
    K.[1,1] <- (3.0*E0*H1 + 3.0*E0*H2*exp(-2.0*b*(B - D)) + G*H1*H2**2.0*a**2.0 + G*H1*H2**2.0*b**2.0 + 6.0*B*E0*H2*b + 3.0*D*E0*H1*b + 3.0*E0*H1*a*dz + 3.0*E0*H2*a*dz*exp(-2.0*b*(B - D)) + G*H1**2.0*H2*a**2.0*exp(-2.0*b*(B - D)) + G*H1**2.0*H2*b**2.0*exp(-2.0*b*(B - D)) + G*H1*H2**2.0*a*b**2.0*dz + 6.0*B*E0*H2*a*b*dz + 6.0*D*E0*H1*a*b*dz + 2.0*B*G*H1**2.0*H2*a**2.0*b + D*G*H1*H2**2.0*a**2.0*b + G*H1**2.0*H2*a*b**2.0*dz*exp(-2.0*b*(B - D)))/(6.0*H1*H2*a*b)
    K.[1,2] <-  -(- D*G*H2**2.0*a**2.0*b - G*H2**2.0*a**2.0 - G*dz*H2**2.0*a*b**2.0 - G*H2**2.0*b**2.0 + 12.0*D*E0*dz*a*b + 6.0*E0*dz*a + 6.0*D*E0*b + 6.0*E0)/(12.0*H2*a*b)
    K.[1,3] <- (D*G*H1*a)/6.0
    K.[1,4] <- 0.0
    K.[2,0] <- 0.0
    K.[2,1] <- -(- D*G*H2**2.0*a**2.0*b - G*H2**2.0*a**2.0 - G*dz*H2**2.0*a*b**2.0 - G*H2**2.0*b**2.0 + 12.0*D*E0*dz*a*b + 6.0*E0*dz*a + 6.0*D*E0*b + 6.0*E0)/(12.0*H2*a*b)
    K.[2,2] <- ((D*G*H2*a**2.0)/3.0 + (D*E0)/H2)/(2.0*a) + ((D*E0)/H3 + (D*G*H2**3.0*a**2.0)/(3.0*H3**2.0))/a + ((G*H2*a**2.0)/3.0 + (G*H2*b**2.0)/3.0 + E0/H2)/(2.0*a*b) + ((G*H3*a**2.0)/3.0 + (G*H3*b**2.0)/3.0 + E0/H3)/(2.0*a*b) + (E0*dz)/(2.0*H2*b) + (E0*dz)/(2.0*H3*b) + (G*H2*b*dz)/6.0 + (G*H3*b*dz)/6.0 + (D*E0*dz)/H2 + (D*E0*dz)/H3
    K.[2,3] <- 0.0
    K.[2,4] <- -(D*G*H2**3.0*a*(3.0*H2 - 4.0*H3))/(6.0*H3**3.0)
    K.[3,0] <- (D*G*H1*a)/6.0
    K.[3,1] <- (D*G*H1*a)/6.0
    K.[3,2] <- 0.0
    K.[3,3] <- (8.0*(G*D**2.0*H1**2.0*a**2.0 + 10.0*E0*dz*D**2.0*a + 10.0*E0*D**2.0 + 3.0*G*dz*H1**2.0*a + 3.0*G*H1**2.0))/(45.0*D*H1*a)
    K.[3,4] <- 0.0
    K.[4,0] <- 0.0
    K.[4,1] <- 0.0
    K.[4,2] <- -(D*G*H2**3.0*a*(3.0*H2 - 4.0*H3))/(6.0*H3**3.0)
    K.[4,3] <- 0.0
    K.[4,4] <- ((16.0*D*E0)/(9.0*H3) + (8.0*G*H3)/(15.0*D) + (8.0*D*G*H2**3.0*a**2.0*(6.0*H2**2.0 - 15.0*H2*H3 + 10.0*H3**2.0))/(45.0*H3**4.0))/a + (16.0*D*E0*dz)/(9.0*H3) + (8.0*G*H3*dz)/(15.0*D)
    K

  /// Stiffness matrix (5 x 5) of simple tunnel model updated with buble functions.
  /// Updated bouderies of integrals of second layer (integration from r)
  let stiffnessMatrix_v3 (E0, G, alpha_x, alpha_z) (H1, H2, H3, B, D, dz, R) =
    let a, b = alpha_z, alpha_x // podle Vladova mailu
    let K = DenseMatrix.zeroCreate 5 5
    K.[0,0] <- (2.0*B*G*H1**2.0*a**2.0*b + G*H1**2.0*a**2.0 + G*dz*H1**2.0*a*b**2.0 + G*H1**2.0*b**2.0 + 6.0*B*E0*dz*a*b + 3.0*E0*dz*a + 6.0*B*E0*b + 3.0*E0)/(6.0*H1*a*b)
    K.[0,1] <- -(exp(-b*(B - D))*(6.0*E0 - G*H1**2.0*a**2.0 - G*H1**2.0*b**2.0 + 6.0*E0*a*dz + 12.0*B*E0*b*exp(b*(B - D)) - G*H1**2.0*a*b**2.0*dz - 2.0*B*G*H1**2.0*a**2.0*b*exp(b*(B - D)) + 12.0*B*E0*a*b*dz*exp(b*(B - D))))/(12.0*H1*a*b)
    K.[0,2] <- 0.0
    K.[0,3] <- (D*G*H1*a)/6.0
    K.[0,4] <- 0.0
    K.[1,0] <- -(exp(-b*(B - D))*(6.0*E0 - G*H1**2.0*a**2.0 - G*H1**2.0*b**2.0 + 6.0*E0*a*dz + 12.0*B*E0*b*exp(b*(B - D)) - G*H1**2.0*a*b**2.0*dz - 2.0*B*G*H1**2.0*a**2.0*b*exp(b*(B - D)) + 12.0*B*E0*a*b*dz*exp(b*(B - D))))/(12.0*H1*a*b)
    K.[1,1] <- (3.0*E0*H2*exp(-2.0*b*(B - D)) + 6.0*B*E0*H2*b + 3.0*D*E0*H1*b + 3.0*E0*H2*a*dz*exp(-2.0*b*(B - D)) + 3.0*E0*H1*a*dz*exp(2.0*b*(D - R)) + G*H1**2.0*H2*a**2.0*exp(-2.0*b*(B - D)) + G*H1**2.0*H2*b**2.0*exp(-2.0*b*(B - D)) + 6.0*B*E0*H2*a*b*dz + 6.0*D*E0*H1*a*b*dz + 2.0*B*G*H1**2.0*H2*a**2.0*b + D*G*H1*H2**2.0*a**2.0*b + G*H1**2.0*H2*a*b**2.0*dz*exp(-2.0*b*(B - D)) + G*H1*H2**2.0*a*b**2.0*dz*exp(2.0*b*(D - R)))/(6.0*H1*H2*a*b)
    K.[1,2] <- ((G*H2*a**2.0)/6.0 + (G*H2*b**2.0)/6.0 - E0/H2)/(4.0*a*b) - ((D*E0)/H2 - (D*G*H2*a**2.0)/6.0)/(2.0*a) + (exp(2.0*b*(D - R))*((G*H2*a**2.0)/6.0 + (G*H2*b**2.0)/6.0 - E0/H2))/(4.0*a*b) - (D*E0*dz)/H2 - (E0*dz*exp(2.0*D*b)*exp(-2.0*R*b))/(2.0*H2*b) + (G*H2*b*dz*exp(2.0*D*b)*exp(-2.0*R*b))/12.0
    K.[1,3] <- (D*G*H1*a)/6.0
    K.[1,4] <- 0.0
    K.[2,0] <- 0.0
    K.[2,1] <- ((G*H2*a**2.0)/6.0 + (G*H2*b**2.0)/6.0 - E0/H2)/(4.0*a*b) - ((D*E0)/H2 - (D*G*H2*a**2.0)/6.0)/(2.0*a) + (exp(2.0*b*(D - R))*((G*H2*a**2.0)/6.0 + (G*H2*b**2.0)/6.0 - E0/H2))/(4.0*a*b) - (D*E0*dz)/H2 - (E0*dz*exp(2.0*D*b)*exp(-2.0*R*b))/(2.0*H2*b) + (G*H2*b*dz*exp(2.0*D*b)*exp(-2.0*R*b))/12.0
    K.[2,2] <- (D*(G*H2**2.0*a**2.0 + 3.0*E0))/(6.0*H2*a) + (D*(G*H2**3.0*a**2.0 + 3.0*E0*H3))/(3.0*H3**2.0*a) + (E0*dz)/(2.0*H3*b) + (G*H2**2.0*a**2.0 + G*H2**2.0*b**2.0 + 3.0*E0)/(12.0*H2*a*b) + (G*H3**2.0*a**2.0 + G*H3**2.0*b**2.0 + 3.0*E0)/(6.0*H3*a*b) + (G*H3*b*dz)/6.0 + (D*E0*dz)/H2 + (D*E0*dz)/H3 + (exp(2.0*b*(D - R))*(G*H2**2.0*a**2.0 + G*H2**2.0*b**2.0 + 3.0*E0))/(12.0*H2*a*b) + (G*H2*b*dz*exp(2.0*b*(D - R)))/6.0 + (E0*dz*exp(2.0*b*(D - R)))/(2.0*H2*b)
    K.[2,3] <- 0.0
    K.[2,4] <- -(D*G*H2**3.0*a*(3.0*H2 - 4.0*H3))/(6.0*H3**3.0)
    K.[3,0] <- (D*G*H1*a)/6.0
    K.[3,1] <- (D*G*H1*a)/6.0
    K.[3,2] <- 0.0
    K.[3,3] <- (8.0*(G*D**2.0*H1**2.0*a**2.0 + 10.0*E0*dz*D**2.0*a + 10.0*E0*D**2.0 + 3.0*G*dz*H1**2.0*a + 3.0*G*H1**2.0))/(45.0*D*H1*a)
    K.[3,4] <- 0.0
    K.[4,0] <- 0.0
    K.[4,1] <- 0.0
    K.[4,2] <- -(D*G*H2**3.0*a*(3.0*H2 - 4.0*H3))/(6.0*H3**3.0)
    K.[4,3] <- 0.0
    K.[4,4] <- ((16.0*D*E0)/(9.0*H3) + (8.0*G*H3)/(15.0*D) + (8.0*D*G*H2**3.0*a**2.0*(6.0*H2**2.0 - 15.0*H2*H3 + 10.0*H3**2.0))/(45.0*H3**4.0))/a + (16.0*D*E0*dz)/(9.0*H3) + (8.0*G*H3*dz)/(15.0*D)
    K

  // Stiffness matrix (6 x 6) of simple tunnel model updated with buble functions.
  // Added horizontal displacement as sixth DoF.
  // DoF: v1, v2, v3, u1, delta v1, delta v2
  let stiffnessMatrix_v4 (E0, G, alpha_x, alpha_z) (H1, H2, H3, B, D, dz, R) =
    let a, b = alpha_z, alpha_x // podle Vladova mailu
    let K = DenseMatrix.zeroCreate 6 6
    K.[0,0] <- (2.0*B*G*H1**2.0*a**2.0*b + G*H1**2.0*a**2.0 + G*dz*H1**2.0*a*b**2.0 + G*H1**2.0*b**2.0 + 6.0*B*E0*dz*a*b + 3.0*E0*dz*a + 6.0*B*E0*b + 3.0*E0)/(6.0*H1*a*b)
    K.[0,1] <- -(exp(-b*(B - D))*(6.0*E0 - G*H1**2.0*a**2.0 - G*H1**2.0*b**2.0 + 6.0*E0*a*dz + 12.0*B*E0*b*exp(b*(B - D)) - G*H1**2.0*a*b**2.0*dz - 2.0*B*G*H1**2.0*a**2.0*b*exp(b*(B - D)) + 12.0*B*E0*a*b*dz*exp(b*(B - D))))/(12.0*H1*a*b)
    K.[0,2] <- 0.0
    K.[0,3] <- 0.0
    K.[0,4] <- (D*G*H1*a)/6.0
    K.[0,5] <- 0.0
    K.[1,0] <- -(exp(-b*(B - D))*(6.0*E0 - G*H1**2.0*a**2.0 - G*H1**2.0*b**2.0 + 6.0*E0*a*dz + 12.0*B*E0*b*exp(b*(B - D)) - G*H1**2.0*a*b**2.0*dz - 2.0*B*G*H1**2.0*a**2.0*b*exp(b*(B - D)) + 12.0*B*E0*a*b*dz*exp(b*(B - D))))/(12.0*H1*a*b)
    K.[1,1] <- (3.0*E0*H2*exp(-2.0*b*(B - D)) + 6.0*B*E0*H2*b + 3.0*D*E0*H1*b + 3.0*E0*H2*a*dz*exp(-2.0*b*(B - D)) + 3.0*E0*H1*a*dz*exp(2.0*b*(D - R)) + G*H1**2.0*H2*a**2.0*exp(-2.0*b*(B - D)) + G*H1**2.0*H2*b**2.0*exp(-2.0*b*(B - D)) + 6.0*B*E0*H2*a*b*dz + 6.0*D*E0*H1*a*b*dz + 2.0*B*G*H1**2.0*H2*a**2.0*b + D*G*H1*H2**2.0*a**2.0*b + G*H1**2.0*H2*a*b**2.0*dz*exp(-2.0*b*(B - D)) + G*H1*H2**2.0*a*b**2.0*dz*exp(2.0*b*(D - R)))/(6.0*H1*H2*a*b)
    K.[1,2] <- ((G*H2*a**2.0)/6.0 + (G*H2*b**2.0)/6.0 - E0/H2)/(4.0*a*b) - ((D*E0)/H2 - (D*G*H2*a**2.0)/6.0)/(2.0*a) + (exp(2.0*b*(D - R))*((G*H2*a**2.0)/6.0 + (G*H2*b**2.0)/6.0 - E0/H2))/(4.0*a*b) - (D*E0*dz)/H2 - (E0*dz*exp(2.0*D*b)*exp(-2.0*R*b))/(2.0*H2*b) + (G*H2*b*dz*exp(2.0*D*b)*exp(-2.0*R*b))/12.0
    K.[1,3] <- (dz/3.0 + 1.0/(3.0*a)) * E0 // Nasobeni E0 jsem pridal. Nejak se tam tuhost projevit musi. Overit!
    K.[1,4] <- (D*G*H1*a)/6.0
    K.[1,5] <- 0.0
    K.[2,0] <- 0.0
    K.[2,1] <- ((G*H2*a**2.0)/6.0 + (G*H2*b**2.0)/6.0 - E0/H2)/(4.0*a*b) - ((D*E0)/H2 - (D*G*H2*a**2.0)/6.0)/(2.0*a) + (exp(2.0*b*(D - R))*((G*H2*a**2.0)/6.0 + (G*H2*b**2.0)/6.0 - E0/H2))/(4.0*a*b) - (D*E0*dz)/H2 - (E0*dz*exp(2.0*D*b)*exp(-2.0*R*b))/(2.0*H2*b) + (G*H2*b*dz*exp(2.0*D*b)*exp(-2.0*R*b))/12.0
    K.[2,2] <- (D*(G*H2**2.0*a**2.0 + 3.0*E0))/(6.0*H2*a) + (D*(G*H2**3.0*a**2.0 + 3.0*E0*H3))/(3.0*H3**2.0*a) + (E0*dz)/(2.0*H3*b) + (G*H2**2.0*a**2.0 + G*H2**2.0*b**2.0 + 3.0*E0)/(12.0*H2*a*b) + (G*H3**2.0*a**2.0 + G*H3**2.0*b**2.0 + 3.0*E0)/(6.0*H3*a*b) + (G*H3*b*dz)/6.0 + (D*E0*dz)/H2 + (D*E0*dz)/H3 + (exp(2.0*b*(D - R))*(G*H2**2.0*a**2.0 + G*H2**2.0*b**2.0 + 3.0*E0))/(12.0*H2*a*b) + (G*H2*b*dz*exp(2.0*b*(D - R)))/6.0 + (E0*dz*exp(2.0*b*(D - R)))/(2.0*H2*b)
    K.[2,3] <- - (dz/3.0 + 1.0/(3.0*a)) * E0 // Nasobeni E0 jsem pridal. Nejak se tam tuhost projevit musi. Overit!
    K.[2,4] <- 0.0
    K.[2,5] <- -(D*G*H2**3.0*a*(3.0*H2 - 4.0*H3))/(6.0*H3**3.0)
    K.[3,0] <- 0.0
    K.[3,1] <- (dz/3.0 + 1.0/(3.0*a)) * E0 // Nasobeni E0 jsem pridal. Nejak se tam tuhost projevit musi. Overit!
    K.[3,2] <- - (dz/3.0 + 1.0/(3.0*a)) * E0 // Nasobeni E0 jsem pridal. Nejak se tam tuhost projevit musi. Overit!
    K.[3,3] <- (4.0*(a*dz + 1.0)*(2.0*G*D**3.0*H2**2.0*a**2.0*b + 20.0*G*D**3.0*b + 6.0*E0*D*H2**2.0*b + 3.0*G*H2**2.0*R**2.0*a**2.0 + 3.0*E0*H2**2.0*R**2.0*b**2.0 + 30.0*G*R**2.0))/(45.0*H2*R**2.0*a*b)
    K.[3,4] <- 0.0
    K.[3,5] <- 0.0
    K.[4,0] <- (D*G*H1*a)/6.0
    K.[4,1] <- (D*G*H1*a)/6.0
    K.[4,2] <- 0.0
    K.[4,3] <- 0.0
    K.[4,4] <- (8.0*(G*D**2.0*H1**2.0*a**2.0 + 10.0*E0*dz*D**2.0*a + 10.0*E0*D**2.0 + 3.0*G*dz*H1**2.0*a + 3.0*G*H1**2.0))/(45.0*D*H1*a)
    K.[4,5] <- 0.0
    K.[5,0] <- 0.0
    K.[5,1] <- 0.0
    K.[5,2] <- -(D*G*H2**3.0*a*(3.0*H2 - 4.0*H3))/(6.0*H3**3.0)
    K.[5,3] <- 0.0
    K.[5,4] <- 0.0
    K.[5,5] <- ((16.0*D*E0)/(9.0*H3) + (8.0*G*H3)/(15.0*D) + (8.0*D*G*H2**3.0*a**2.0*(6.0*H2**2.0 - 15.0*H2*H3 + 10.0*H3**2.0))/(45.0*H3**4.0))/a + (16.0*D*E0*dz)/(9.0*H3) + (8.0*G*H3*dz)/(15.0*D)
    K

  /// Creates a load vector 5x1 representing the excvation forces.
  let loadVector_v2 (gamma, h0, r, deltaZ) =
    let Pi = System.Math.PI
    let F = Pi / 2.0 * gamma * h0 * r * deltaZ
    DenseMatrix.ofArray2(array2D [[0.0; -F; F; 0.0; 0.0]]).Transpose() 
  let loadVector_v3 = loadVector_v2

  /// Creates a load vector 6x1 representing the excvation forces.
  /// DoF: v1, v2, v3, u1, delta v1, delta v2
  let loadVector_v4 (gamma, h0, r, deltaZ, K0) =
    let Pi = System.Math.PI
    let Fy = Pi / 2.0 * gamma * h0 * r * deltaZ
    let Fx = K0 * Fy
    DenseMatrix.ofArray2(array2D [[0.0; -Fy; Fy; Fx; 0.0; 0.0]]).Transpose() 

  // ==== MODELS ====
  // Variant 2: Independent bubbles, 5 DOF
  let model_v2 (E, nu, gamma) (H1, H2, H3, r, dz) (B, D, alpha_x, alpha_z) =
    let E0, G = edomModulus E nu, shearModulus E nu 
    let K = stiffnessMatrix_v2 (E0, G, alpha_x, alpha_z) (H1, H2, H3, B, D, dz) 
    let h0 = H1 + H2 / 2.0
    let F = loadVector_v2 (gamma, h0, r, dz)
    (K.Inverse() * F) |> vecToList |> List.map (totalDisp alpha_z dz)

  // Variant 3: Given approximation of deltaV1 and deltaV2 using kappa1 and kappa2
  // Lagrange multipliers, condensed to 3 DOF
  let getA kappa_1 kappa_2 =
    DenseMatrix.ofArray2(array2D [[-kappa_1 / 4.0; kappa_1 / 4.0; 0.0]; [0.0; 0.0; kappa_2 / 4.0]]) 
  let condenseK (K:Matrix<float>) (A:Matrix<float>) =
    let K11 = K.SubMatrix(0, 3, 0, 3)
    let K12 = K.SubMatrix(0, 3, 3, 2)
    let K22 = K.SubMatrix(3, 2, 3, 2)
    K11 + K12 * A + A.Transpose() * (K12.Transpose() + K22 * A)
  let condenseF (F:Matrix<float>) =
    F.SubMatrix(0, 3, 0, 1)
  let model_v3 (E, nu, gamma) (H1, H2, H3, r, dz) (B, D, alpha_x, alpha_z, kappa_1, kappa_2) =
    let E0, G = edomModulus E nu, shearModulus E nu 
    let K = stiffnessMatrix_v2 (E0, G, alpha_x, alpha_z) (H1, H2, H3, B, D, dz)
    let A = getA kappa_1 kappa_2
    let K_c = condenseK K A
    let h0 = H1 + H2 / 2.0
    let F = loadVector_v2 (gamma, h0, r, dz)
    let F_c = condenseF F
    (K_c.Inverse() * F_c) |> vecToList |> List.map (totalDisp alpha_z dz)

  // Variant 4: Given approximation of deltaV1 and deltaV2 using kappa1 and kappa2
  // Lagrange multipliers, all 7 DoF 3+2+2
  let model_v4 (E, nu, gamma) (H1, H2, H3, R, dz) (B, D, alpha_x, alpha_z, kappa_1, kappa_2) =
    let E0, G = edomModulus E nu, shearModulus E nu 
    let K = stiffnessMatrix_v3 (E0, G, alpha_x, alpha_z) (H1, H2, H3, B, D, dz, R)
    let A = getA kappa_1 kappa_2
    let lowerMatrix = (-A).Append(DenseMatrix.Identity(2))
    let leftMatrix = K.Stack(lowerMatrix)
    let rightMatrix = (lowerMatrix.Transpose()).Stack(DenseMatrix.zeroCreate 2 2)
    let K_extended = leftMatrix.Append(rightMatrix)
    let h0 = H1 + H2 / 2.0
    let F = loadVector_v3 (gamma, h0, R, dz)
    let F_extended = F.Stack(DenseMatrix.zeroCreate 2 1)
    (K_extended.Inverse() * F_extended) |> vecToList |> List.map (totalDisp alpha_z dz)

  // Variant 5: Given approximation of deltaV1 and deltaV2 using kappa1 and kappa2
  // Lagrange multipliers, all 8 DoFs 4+2+2
  let model_v5 (E, nu, gamma) (H1, H2, H3, R, dz) (B, D, alpha_x, alpha_z, kappa_1, kappa_2) =
    let E0, G = edomModulus E nu, shearModulus E nu 
    let K = stiffnessMatrix_v4 (E0, G, alpha_x, alpha_z) (H1, H2, H3, B, D, dz, R)
    let A = DenseMatrix.ofArray2(array2D [[-kappa_1 / 4.0; kappa_1 / 4.0; 0.0; 0.0]; [0.0; 0.0; kappa_2 / 4.0; 0.0]]) 
    let lowerMatrix = (-A).Append(DenseMatrix.Identity(2))
    let leftMatrix = K.Stack(lowerMatrix)
    let rightMatrix = (lowerMatrix.Transpose()).Stack(DenseMatrix.zeroCreate 2 2)
    let K_extended = leftMatrix.Append(rightMatrix)
    let h0 = H1 + H2 / 2.0
    let K0 = nu / (1.0 - nu)
    let F = loadVector_v4 (gamma, h0, R, dz, K0)
    let F_extended = F.Stack(DenseMatrix.zeroCreate 2 1)
    (K_extended.Inverse() * F_extended) |> vecToList |> List.map (totalDisp alpha_z dz)

  // ==== Module TESTS ====
  module Tests =
    let stiffnessMatrix_TEST () =
      let (E, nu, alpha) = (20.0, 0.3, 0.1)
      let (h1, h2, h3, L, r) = (20.0, 8.0, 50.0, 100.0, 4.0)
      let K = stiffnessMatrix (E, nu, alpha) (h1, h2, h3, L, r)
      let K_inv = K.Inverse()
      let printRhsAndSol R =
        let r = K_inv * R
        printfn "Vector R = %A" R
        printfn "Vector r = %A" r
      printfn "(E, nu, alpha) = %A" (E, nu, alpha)   
      printfn "(h1, h2, h3, L, r) = %A" (h1, h2, h3, L, r)
      printfn "Matrix K = %A" K
      printfn "Matrix K_inv = %A" K_inv
      printRhsAndSol (DenseMatrix.ofArray2(array2D [[1.0]; [0.0]; [0.0]]))
      printRhsAndSol (DenseMatrix.ofArray2(array2D [[0.0]; [1.0]; [0.0]]))
      printRhsAndSol (DenseMatrix.ofArray2(array2D [[0.0]; [0.0]; [1.0]]))
      printRhsAndSol (DenseMatrix.ofArray2(array2D [[0.0]; [-1.0]; [1.0]]))

    // Note:
    // Sum_(i=0..inf) (exp(-c*i) = exp(c)/(exp(c)-1.0) 
    // Numerical test of this relation
    let testOfExpSeriesSum c = // Constant c should be positive. The large c is the faster the series converges. 
      let sum_v1 = exp(c) / (exp(c) - 1.0) 
      let sum_v2 = [0..1000] |> List.sumBy (fun i -> exp(-c * double i))
      sum_v2 - sum_v1
    //let res = test01 1.23

    let test03() =
      // Material [kN, m]
      let (E, nu, alpha, gamma) = (15000.0, 0.35, 0.1, 18.0) // Gamma denotes self weight.
      // Geometry [m]
      let (h1, h2, h3, L, r, deltaZ) = (20.0, 8.0, 12.0, 10.0, 4.0, 1.5)
      let h0 = h1 + h2 / 2.0
      // Stiffness matrix
      let K = stiffnessMatrix (E, nu, alpha) (h1, h2, h3, L, r)
      let K_inv = K.Inverse()
      // Load vector
      let F = loadVector(gamma, h0, r, deltaZ)
      // Solution: vector of nodal incremental displacements
      let deltaV = K_inv * F
      let vectorToList (x:Matrix<float>) =
        x.Column(0).ToArray() |> Array.toList
      // Total nodal displacements. Computed as a sum of all incremental displacements.
      let totalV = deltaV |> vectorToList |> List.map (totalDisp alpha deltaZ)
    
      // Printing
      printfn "TEST 03"
      printfn "(E, nu, alpha, gamma) = %A" (E, nu, alpha, gamma)   
      printfn "(h1, h2, h3, L, r, deltaZ) = %A" (h1, h2, h3, L, r, deltaZ)
      printfn "Matrix K = %A" K
      printfn "Matrix K_inv = %A" K_inv
      printfn "Vector F = %A" F
      printfn "Vector deltaV = %A" deltaV
      printfn "Vector totalV = %A" totalV
      () 

    let test04 () =
      // Material [kN, m]
      let (E, nu) = (15000.0, 0.35)
      let E0 = edomModulus E nu
      let G = shearModulus E nu
      // Podle mailu 28.2.2014 plati a = alpha_z b = alpha_x
      let (alpha_x, alpha_z) = (0.3, 0.1)
      let gamma = 18.0
      // Geometry
      let (H1, H2, H3, B, D) = (20.0, 8.0, 12.0, 8.0, 3.0)
      let h0 = H1 + H2 / 2.0
      let (r, deltaZ) = (4.0, 1.5)
      // Stiffness matrix
      let K = stiffnessMatrix_v2 (E0, G, alpha_x, alpha_z) (H1, H2, H3, B, D, deltaZ)
      let K_inv = K.Inverse()
      // Load vector
      let F = loadVector_v2(gamma, h0, r, deltaZ)
      // Solution: vector of nodal incremental displacements
      let deltaV = K_inv * F
      let vectorToList (x:Matrix<float>) =
        x.Column(0).ToArray() |> Array.toList
      // Total nodal displacements. Computed as a sum of all incremental displacements.
      let totalV = deltaV |> vectorToList |> List.map (totalDisp alpha_z deltaZ)

      // Submatrices
      let Kaa = K.SubMatrix(0, 3, 0, 3)
      let Kab = K.SubMatrix(0, 3, 3, 2)
      let F_v1 = F.SubMatrix(0, 3, 0, 1)
      let kappa1, kappa2 = 1.0, 1.0
      // Variant 1: Matrix that expresses deltaV1 and deltaV2 in terms of V1, V2 and V3
      let GetA k1 k2 = DenseMatrix.ofArray2(array2D [[-k1 / 4.0; k1 / 4.0; 0.0]; [0.0; 0.0; k2 / 4.0]]) 
      let A = GetA kappa1 kappa2
      let K_v1 = Kaa + Kab * A
      let K_v1_inv = K_v1.Inverse()
      let r_v1 = K_v1_inv * F_v1
      // Variant 2: Make the stiffness matrix symmetric
      let K_v2 = K_v1.Transpose() * K_v1
      let F_v2 = K_v1.Transpose() * F_v1
      let K_v2_inv = K_v2.Inverse()
      let r_v2 = K_v2_inv * F_v2
      // Variant 3: Lagrange multipliers
      let Kbb = K.SubMatrix(3, 2, 3, 2)
      let K_v3 = (Kaa + Kab * A + A.Transpose() * (Kab.Transpose() - Kbb * A))
      let K_v3_inv = K_v3.Inverse()
      let r_v3 = K_v3_inv * F_v1 // The load vector is identical to verison 1.
    
      // Printing
      log "TEST 04"
      log "(E, nu, alpha_x, alpha_z, gamma) = %A" (E, nu, alpha_x, alpha_z, gamma)   
      log "(E0, G) = %A" (E0, G)
      log "(h1, h2, h3, r, deltaZ) = %A" (H1, H2, H3, r, deltaZ)
      log "(B, D, h0) = %A" (B, D, h0)
      log "Matrix K = %A" K
      log "Matrix K is symmetric = %A" K.IsSymmetric
      log "Matrix K_inv = %A" K_inv
      log "Vector F = %A" F
      log "Vector deltaV = %A" deltaV
      log "Vector totalV = %A" totalV
      log ""
      log "-- v1 --"
      log "Matrix Kaa = %A" Kaa
      log "Matrix Kab = %A" Kab
      log "Matrix K_v1 = %A" K_v1
      log "Vector F_v1 = %A" F_v1
      log "Vector r_v1 = %A" r_v1
  //    log ""
  //    log "-- v2 --"
  //    log "Matrix K_v2 = %A" K_v2
  //    log "Vector F_v2 = %A" F_v2
  //    log "Vector r_v2 = %A" r_v2
      log ""
      log "-- v3 --"
      log "Matrix K_v3 = %A" K_v3
      log "Vector F_v3 = %A" F_v1
      log "Vector r_v3 = %A" r_v3

      let v1, v2, v3 = totalV.[0], totalV.[1], totalV.[2]
      let delta_v1, delta_v3 = totalV.[3], totalV.[4]
      let v3_d = v3 / 2.0 + delta_v3
      let v1_d = (v1 + v2) / 2.0 + delta_v1

      // Points of vertical displacement from bottom up. 
      let points = [(0.0, 0.0); (H3/2.0, v3_d); (H3, v3); (H3 + H2, v2); (H3 + H2 + H1 / 2.0, v1_d); (H3 + H2 + H1, v1)]
      // Values copied from "SimpleTunel\Test.gmk"
      let points_fem = [(0.0, 0.0); (6.0, -42.2); (12.0, -155.0); (20.0, 179.0); (30.0, 85.3); (40.0, 66.1)] |> List.map (fun (a,b) -> (a, -b/1000.0))
      let myChart = Chart.Combine [Chart.Line (points, "Simple");  Chart.Line (points_fem, "FEM")]
      //let myChart = [for x in 0 .. 10 -> x, x * x] |> Chart.Line
      let form = new Form(Visible = true, TopMost = true, Width = 700, Height = 500)
      form.Controls.Add(new ChartTypes.ChartControl(myChart, Dock = DockStyle.Fill))
      Application.Run(form);
      ()

    let test05 () =
      // Material (fixed)
      let (E, nu, gamma) = (15000.0, 0.35, 18.0)
      let material = (E, nu, gamma)
      // Geometry (fixed)
      let (H1, H2, H3, r, dz) = (20.0, 8.0, 12.0, 4.0, 1.5)
      let geometry = (H1, H2, H3, r, dz)
      // Free params
      let (B, D, alpha_x, alpha_z, kappa_1, kappa_2) = (8.0, 3.0, 0.3, 0.1, 1.0, 1.0)
    
      // Varianta 2
      let r_v2 = model_v2 material geometry (B, D, alpha_x, alpha_z)
      // Varianta 3
      let r_v3 = model_v3 material geometry (B, D, alpha_x, alpha_z, kappa_1, kappa_2)
      // Varianta 4
      let r_v4 = model_v4 material geometry (B, D, alpha_x, alpha_z, kappa_1, kappa_2)

      // Points from bottom up, i.e. botom is zero, terrain is positive.
      let ys = [0.0; H3; H3 + H2; H3 + H2 + H1]
      let ys_fine = [0.0; H3 / 2.0; H3; H3 + H2; H3 + H2 + H1 / 2.0; H3 + H2 + H1]

      // Computes displacement in points from bottom up.
      let aux (r:float list) = [0.0; r.[2] / 2.0 - r.[4]; r.[2]; r.[1]; (r.[1] + r.[0]) / 2.0 - r.[3]; r.[0]]

      let points_v2 = (aux r_v2) |> List.zip ys_fine
      let points_v3 = [0.0; r_v3.[2]; r_v3.[1]; r_v3.[0]] |> List.zip ys
      let points_v4 = (aux r_v4) |> List.zip ys_fine
      let points_FEM = [(0.0, 0.0); (6.0, -42.2); (12.0, -155.0); (20.0, 179.0); (30.0, 85.3); (40.0, 66.1)] |> List.map (fun (a,b) -> (a, -b / 1000.0))

      let myChart = Chart.Combine [Chart.Line (points_FEM, "FEM"); Chart.Line (points_v2, "Var 2");  Chart.Line (points_v3, "Var 3"); Chart.Line (points_v4, "Var 4")]
      //let myChart = [for x in 0 .. 10 -> x, x * x] |> Chart.Line
      let form = new Form(Visible = true, TopMost = true, Width = 700, Height = 500)
      form.Controls.Add(new ChartTypes.ChartControl(myChart, Dock = DockStyle.Fill))
      Application.Run(form);
      ()
        
    let test06 () =
      // Material (fixed)
      let (E, nu, gamma) = (15000.0, 0.35, 18.0)
      let material = (E, nu, gamma)
      // Geometry (fixed)
      let (H1, H2, H3, R, dz) = (20.0, 8.0, 12.0, 3.5, 1.5)
      let geometry = (H1, H2, H3, R, dz)
      let alpha_z = 0.1
      // Bounds of free parameters (B, D, alpha_x, kappa_1, kappa_2)
      let bounds = [(5.0, 20.0); (0.0, 4.0); (0.05, 1.0); (0.99, 1.0); (0.99, 1.0)]

      // Computes displacement in points from bottom up.
      let aux (r:float list) = [0.0; r.[2] / 2.0 - r.[4]; r.[2]; r.[1]; (r.[1] + r.[0]) / 2.0 - r.[3]; r.[0]]

      // Objective function. Version 1: Five vertical displacements are compared with FEM model.
      let genObjFunc_v1 material geometry alpha_z (pars:float list) =
        let r_v4 = model_v4 material geometry (pars.[0], pars.[1], pars.[2], alpha_z, pars.[3], pars.[4])
        let data_fem = [0.0; -42.2; -155.0; 179.0; 85.3; 66.1] |> List.map (fun x -> -x / 1000.0)
        OptimModule.sumOfSquares (aux r_v4) data_fem

      // Objective function. Version 2: Only three vertical displacements are compared with FEM model.
      let genObjFunc_v2 material geometry alpha_z (pars:float list) =
        let r_v4 = model_v4 material geometry (pars.[0], pars.[1], pars.[2], alpha_z, pars.[3], pars.[4])
        let data_fem = [-155.0; 179.0; 66.1] |> List.map (fun x -> -x / 1000.0)
        let data_v4 = [2; 1; 0] |> List.map (fun i -> r_v4.[i])
        OptimModule.sumOfSquares data_v4 data_fem
    
      // Objective function. Version 3: Only three vertical displacements are compared with FEM model.
      // Alpha_x = alpha_z
      let genObjFunc_v3 material geometry (pars:float list) =
        let r_v4 = model_v4 material geometry (pars.[0], pars.[1], pars.[2], pars.[2], pars.[3], pars.[4])
        let data_fem = [-155.0; 179.0; 66.1] |> List.map (fun x -> -x / 1000.0)
        let data_v4 = [2; 1; 0] |> List.map (fun i -> r_v4.[i])
        OptimModule.sumOfSquares data_v4 data_fem

      // Partialy apply fixed parameters
      let objFunc = genObjFunc_v3 material geometry

      let best = OptimModule.optimize_bestRandom objFunc bounds 100000
      let par = fst best

      log "Test 06"
      log "-- Random optimizer --"
      log "Params = %A" par
      log "Error = %A" (snd best)

      let r = model_v4 material geometry (par.[0], par.[1], par.[2], par.[2], par.[3], par.[4])
      let ys = [0.0; H3 / 2.0; H3; H3 + H2; H3 + H2 + H1 / 2.0; H3 + H2 + H1]

      // Computes displacement in points from bottom up.
      let points_v4 = (aux r) |> List.zip ys
      let points_FEM = [(0.0, 0.0); (6.0, -42.2); (12.0, -155.0); (20.0, 179.0); (30.0, 85.3); (40.0, 66.1)] |> List.map (fun (a,b) -> (a, -b / 1000.0))

      let myChart = Chart.Combine [Chart.Line (points_FEM, "FEM"); Chart.Line (points_v4, "Var 4")]
      //let myChart = [for x in 0 .. 10 -> x, x * x] |> Chart.Line
      let form = new Form(Visible = true, TopMost = true, Width = 700, Height = 500)
      form.Controls.Add(new ChartTypes.ChartControl(myChart, Dock = DockStyle.Fill))
      Application.Run(form);
      ()
  
    // Plot vertical displacements with optimal params
    let test07 () =
      // Material (fixed)
      let (E, nu, gamma) = (15000.0, 0.35, 18.0)
      let material = (E, nu, gamma)
      // Geometry (fixed)
      let (H1, H2, H3, R, dz) = (20.0, 8.0, 12.0, 3.5, 1.5)
      let geometry = (H1, H2, H3, R, dz)
      // Fixed
      let (kappa_1, kappa_2) = (1.0, 1.0)

      let par = [6.353570591; 0.5152510053; 0.1865739747]
      let r = model_v4 material geometry (par.[0], par.[1], par.[2], par.[2], kappa_1, kappa_2)
      let ys = [0.0; H3 / 2.0; H3; H3 + H2; H3 + H2 + H1 / 2.0; H3 + H2 + H1]

      // Computes displacement in points from bottom up.
      let aux (r:float list) = [0.0; r.[2] / 2.0 - r.[4]; r.[2]; r.[1]; (r.[1] + r.[0]) / 2.0 - r.[3]; r.[0]]

      // Computes displacement in points from bottom up.
      let points_v4 = (aux r) |> List.zip ys
      let points_FEM = [(0.0, 0.0); (6.0, -42.2); (12.0, -155.0); (20.0, 179.0); (30.0, 85.3); (40.0, 66.1)] |> List.map (fun (a,b) -> (a, -b / 1000.0))

      log "FEM %A" points_FEM
      log "Model %A" points_v4
      log ""
      log "Y, FEM, Model"
      List.iter2 (fun x y -> log "%A, %A, %A" (fst x) (snd x) (snd y)) points_FEM points_v4

      let myChart = Chart.Combine [Chart.Line (points_FEM, "FEM"); Chart.Line (points_v4, "Var 4")]
      //let myChart = [for x in 0 .. 10 -> x, x * x] |> Chart.Line
      let form = new Form(Visible = true, TopMost = true, Width = 700, Height = 500)
      form.Controls.Add(new ChartTypes.ChartControl(myChart, Dock = DockStyle.Fill))
      Application.Run(form);
      ()
    
     // Params = [10.42270456; 0.3537436763; 0.1456818726; 0.5827337432; 0.6011374665]
     // Params = [6.353570591; 0.5152510053; 0.1865739747; 0.9962843907; 0.9915528451] OK

    let test08 () =
      // Material (fixed)
      let (E, nu, gamma) = (15000.0, 0.35, 18.0)
      let material = (E, nu, gamma)
      // Geometry (fixed)
      let (H1, H2, H3, R, dz) = (20.0, 8.0, 12.0, 3.5, 1.5)
      let geometry = (H1, H2, H3, R, dz)
      let alpha_z = 0.1
      // Bounds of free parameters (B, D, alpha_x, kappa_1, kappa_2)
      let bounds = [(5.0, 20.0); (0.0, 4.0); (0.05, 1.0); (0.99, 1.0); (0.99, 1.0)]

      // Computes displacement in points from bottom up.
      let aux (r:float list) = [0.0; r.[2] / 2.0 - r.[5]; r.[2]; r.[1]; (r.[1] + r.[0]) / 2.0 - r.[4]; r.[0]]

      // Objective function. Version 3: Only three vertical displacements are compared with FEM model.
      // Alpha_x = alpha_z
      let genObjFunc material geometry (par:float list) =
        let r_v5 = model_v5 material geometry (par.[0], par.[1], par.[2], par.[2], par.[3], par.[4])
        let data_fem = [-155.0; 179.0; 69.1] |> List.map (fun x -> -x / 1000.0)
        let data_v5 = [2; 1; 0] |> List.map (fun i -> r_v5.[i])
        OptimModule.sumOfSquares data_v5 data_fem

      // Partialy apply fixed parameters
      let objFunc = genObjFunc material geometry

      let best = OptimModule.optimize_bestRandom objFunc bounds 10000
      let par = fst best

      log "Test 08"
      log "-- Random optimizer --"
      log "Params = %A" par
      log "Error = %A" (snd best)

      let r = model_v5 material geometry (par.[0], par.[1], par.[2], par.[2], par.[3], par.[4])
      let ys = [0.0; H3 / 2.0; H3; H3 + H2; H3 + H2 + H1 / 2.0; H3 + H2 + H1]
      log "Horizontal displacement u1 = %A" r.[3]

      // Computes displacement in points from bottom up.
      let points_v4 = (aux r) |> List.zip ys
      let points_FEM = [(0.0, 0.0); (6.0, -42.2); (12.0, -155.0); (20.0, 179.0); (30.0, 85.3); (40.0, 66.1)] |> List.map (fun (a,b) -> (a, -b / 1000.0))

      let myChart = Chart.Combine [Chart.Line (points_FEM, "FEM"); Chart.Line (points_v4, "Var 4")]
      //let myChart = [for x in 0 .. 10 -> x, x * x] |> Chart.Line
      let form = new Form(Visible = true, TopMost = true, Width = 700, Height = 500)
      form.Controls.Add(new ChartTypes.ChartControl(myChart, Dock = DockStyle.Fill))
      Application.Run(form);
      ()
  
