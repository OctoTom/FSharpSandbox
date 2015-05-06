namespace MicroMech

open MathNet.Numerics.LinearAlgebra.Double
open MathNet.Numerics.LinearAlgebra.Generic


module Rotation =

  // Creates a matrix of list of lists. The lists are the rows.
  let listToMatrix list =
        DenseMatrix.ofArray2( array2D list)

  // Computes a transormation matrix for a 3D space fro rotation around z axis
  // The matrix transrorms from global to local coordinates.
  // The angle goes from global x to local x.
  let getTransMatrix angle =
    let c = cos angle
    let s = sin angle
    DenseMatrix.ofArray2( array2D [[c; s; 0.0]; [-s; c; 0.0]; [0.0; 0.0; 1.0]])
  
  // Function copied from muMech
  // Creates a 6x6 matrix that transforms tensors writne as 6x1 vectors. a' = T6x6 * a
  let getTransMatrix_6x6 (T:DenseMatrix) =
    let _T (i, j) = T.[i-1, j-1]
    let SQR x = x * x
    let Ts = DenseMatrix.zeroCreate 6 6
     
    Ts.[0, 0] <- SQR( _T( 1, 1 ) );
    Ts.[0, 1] <- SQR( _T( 1, 2 ) );
    Ts.[0, 2] <- SQR( _T( 1, 3 ) );
    Ts.[0, 3] <- 2. * _T( 1, 1 ) * _T( 1, 2 );//s_12 transform
    Ts.[0, 4] <- 2. * _T( 1, 2 ) * _T( 1, 3 );//s_23 transform
    Ts.[0, 5] <- 2. * _T( 1, 1 ) * _T( 1, 3 );//s_13 transform

    Ts.[1, 0] <- SQR( _T( 2, 1 ) );
    Ts.[1, 1] <- SQR( _T( 2, 2 ) );
    Ts.[1, 2] <- SQR( _T( 2, 3 ) );
    Ts.[1, 3] <-  2. * _T( 2, 1 ) * _T( 2, 2 );//s_12 transform
    Ts.[1, 4] <- 2. * _T( 2, 2 ) * _T( 2, 3 );//s_23 transform
    Ts.[1, 5] <- 2. * _T( 2, 1 ) * _T( 2, 3 );//s_13 transform

    Ts.[2, 0] <- SQR( _T( 3, 1 ) );
    Ts.[2, 1] <- SQR( _T( 3, 2 ) );
    Ts.[2, 2] <- SQR( _T( 3, 3 ) );
    Ts.[2, 3] <- 2. * _T( 3, 1 ) * _T( 3, 2 );//s_12 transform
    Ts.[2, 4] <- 2. * _T( 3, 2 ) * _T( 3, 3 );//s_23 transform
    Ts.[2, 5] <- 2. * _T( 3, 1 ) * _T( 3, 3 );//s_13 transform

    Ts.[3, 0] <- _T( 2, 1 ) * _T( 1, 1 );
    Ts.[3, 1] <- _T( 2, 2 ) * _T( 1, 2 );
    Ts.[3, 2] <- _T( 2, 3 ) * _T( 1, 3 );
    Ts.[3, 3] <- _T( 1, 1 ) * _T( 2, 2 ) + _T( 2, 1 ) * _T( 1, 2 );//s_12 transform
    Ts.[3, 4] <- _T( 2, 3 ) * _T( 1, 2 ) + _T( 2, 2 ) * _T( 1, 3 );//s_23 transform
    Ts.[3, 5] <- _T( 2, 3 ) * _T( 1, 1 ) + _T( 2, 1 ) * _T( 1, 3 );//s_13 transform

    Ts.[4, 0] <- _T( 3, 1 ) * _T( 2, 1 );
    Ts.[4, 1] <- _T( 3, 2 ) * _T( 2, 2 );
    Ts.[4, 2] <- _T( 3, 3 ) * _T( 2, 3 );
    Ts.[4, 3] <- _T( 3, 1 ) * _T( 2, 2 ) + _T( 3, 2 ) * _T( 2, 1 );//s_12 transform
    Ts.[4, 4] <- _T( 3, 2 ) * _T( 2, 3 ) + _T( 3, 3 ) * _T( 2, 2 );//s_23 transform
    Ts.[4, 5] <- _T( 3, 3 ) * _T( 2, 1 ) + _T( 3, 1 ) * _T( 2, 3 );//s_13 transform

    Ts.[5, 0] <- _T( 3, 1 ) * _T( 1, 1 );
    Ts.[5, 1] <- _T( 3, 2 ) * _T( 1, 2 );
    Ts.[5, 2] <- _T( 3, 3 ) * _T( 1, 3 );
    Ts.[5, 3] <- _T( 3, 2 ) * _T( 1, 1 ) + _T( 3, 1 ) * _T( 1, 2 );//s_12 transform
    Ts.[5, 4] <- _T( 3, 2 ) * _T( 1, 3 ) + _T( 3, 3 ) * _T( 1, 2 );//s_23 transform
    Ts.[5, 5] <- _T( 3, 3 ) * _T( 1, 1 ) + _T( 3, 1 ) * _T( 1, 3 );//s_13 transform
    Ts

  let globalToLocal_vec (T:DenseMatrix) (vec:DenseMatrix) = T * vec
  let localToGlobal_vec (T:DenseMatrix) (vec:DenseMatrix) = T.Transpose() * vec
  let globalToLocal_tens (T:DenseMatrix) (tens:DenseMatrix) = T * tens * T.Transpose()
  let localToGlobal_tens (T:DenseMatrix) (tens:DenseMatrix) = T.Transpose() * tens * T
  let globalToLocal_strain (T:DenseMatrix) (vec:DenseMatrix) =
    let T6x6 = getTransMatrix_6x6 T
    T6x6 * vec
  let localToGlobal_strain (T:DenseMatrix) (vec:DenseMatrix) =
    let T6x6 = getTransMatrix_6x6 (T.Transpose() |> DenseMatrix.OfMatrix)
    T6x6 * vec

  let toDeg rad = 180.0 / System.Math.PI * rad

  let listToVec list =
    DenseMatrix.ofArray2( array2D [list]).Transpose() |> DenseMatrix.OfMatrix

  let tensToVec (tens:Matrix) =
    [tens.[0, 0]; tens.[1, 1]; tens.[2, 2]; tens.[0, 1]; tens.[1, 2]; tens.[2, 0]] |> listToVec |> DenseMatrix.OfMatrix

  let vecToTens (vec:Matrix) =
    DenseMatrix.ofArray2( array2D [[vec.[0, 0]; vec.[3, 0]; vec.[5, 0]]; [vec.[3, 0]; vec.[1, 0]; vec.[4, 0]]; [vec.[5, 0]; vec.[4, 0]; vec.[2, 0]]]).Transpose() |> DenseMatrix.OfMatrix

  ////////////////////////////////////////
  // Pootocena inkluze resena v ANSYSu. //
  ////////////////////////////////////////
  let test01 () =
    // Hlavni poloosa rotovane inkluze smeruje do bodu [x, y]
    let x, y = 1.0, 0.8
  
    // Uhel od kladne poloosy x
    let theta = atan (y / x)

    // Theta ve stupnich
    let theta_deg = toDeg theta

    // Transormacni matice 3x3
    let T = getTransMatrix theta

    // Some point in global coords.
    // Node 5718
    //let x_g = [0.421920885999; 0.788437457315; 0.0] |> listToVec
    // Node 5755
    //let x_g = [0.478191788745; -0.382610092424; 0.0] |> listToVec
    // Node 40058
    let x_g = [1.17772865920; 1.12477850092; 0.0] |> listToVec
    let x_l = T * x_g

    let normal_strain_g = [[1.0; 0.0; 0.0]; [0.0; 0.0; 0.0]; [0.0; 0.0; 0.0]] |> listToMatrix
    let shear_strain_g = [[0.0; 1.0; 0.0]; [1.0; 0.0; 0.0]; [0.0; 0.0; 0.0]] |> listToMatrix
    let normal_strain_l = globalToLocal_tens T normal_strain_g
    let shear_strain_l = globalToLocal_tens T shear_strain_g

    // Node 5718: e2 = -0.101834, -0.00144743, 0, 0.170388, 0, 0,
  //  let ansys_strain_l = listToMatrix [[-0.101834; 0.170388; 0.0]; [0.170388; -0.00144743; 0.0]; [0.0; 0.0; 0.0]]
    // Node 5755: e2 = -0.0608311, 0.0526654, 0, -0.102462, 0, 0;
  //  let ansys_strain_l = listToMatrix [[-0.0608311; -0.102462; 0.0]; [-0.102462; 0.0526654; 0.0]; [0.0; 0.0; 0.0]]
    // Node 40058: e2 = 0.0153424, -0.0127207, 0, 0.0697787, 0, 0,
    let ansys_strain_l = listToMatrix [[0.0153424; 0.0697787; 0.0]; [0.0697787; -0.0127207; 0.0]; [0.0; 0.0; 0.0]]
  
    let ansys_strain_g = localToGlobal_tens T ansys_strain_l

    // Equivalent strain
    // -3.91881748e-001 3.86932603e-001 0.00000000e+000 3.86932603e-001 -1.47305054e-001 0.00000000e+000 0.00000000e+000 0.00000000e+000 3.76176839e-001
    let eq_loc = listToMatrix [[-3.91881748e-001; 3.86932603e-001; 0.00000000e+000]; [3.86932603e-001; -1.47305054e-001; 0.00000000e+000]; [0.00000000e+000; 0.00000000e+000; 3.76176839e-001]]
    let eq_glob = localToGlobal_tens T eq_loc

    let T6x6 = getTransMatrix_6x6 T
    let T6x6T_v1 = getTransMatrix_6x6 (T6x6.Transpose() |> DenseMatrix.OfMatrix)
    let T6x6T_v2 = (getTransMatrix_6x6 T).Transpose()



    printfn "T = %A" T
    printfn "x_g = %A" x_g
    printfn "x_l = %A" x_l
    printfn "normal_strain_g = %A" normal_strain_g
    printfn "shear_strain_g = %A" shear_strain_g
    printfn "normal_strain_l = %A" normal_strain_l
    printfn "shear_strain_l = %A" shear_strain_l
    printfn "ansys_strain_g = %A" ansys_strain_g
    //printfn "normal_strain_g_check = %A" (localToGlobal_tens T (DenseMatrix.OfMatrix normal_strain_l))
    //printfn "shear_strain_g_check = %A" (localToGlobal_tens T (DenseMatrix.OfMatrix shear_strain_l))
    printfn "eq_loc = %A" eq_loc
    printfn "eq_glob = %A" eq_glob
    printfn "eq_loc.Det = %A" (eq_loc.Determinant())
    printfn "eq_glob.Det = %A" (eq_glob.Determinant())
    printfn "----"
    printfn "eq_loc = %A" eq_loc
    printfn "eq_loc = %A" (eq_loc |> tensToVec)
    printfn "eq_loc = %A" (eq_loc |> tensToVec |> vecToTens)
    printfn "----"
    printfn "eq_loc = %A" eq_loc



    let ansys_load = [1.0; 0.0; 0.0; 0.0; 0.0; 0.0]
    let ansys_sol = [0.78994; 0.12416; 0.0000; -0.20699E-01; 0.0; 0.0]
    let ansys_pert = List.map2 (-) ansys_sol ansys_load
  
    let mumech_pert = [-0.227627; 0.082078; 0.0; -0.0650838; 0.0; 0.0]

    let res1 = List.map2 (/) mumech_pert ansys_pert

    let matrix = DenseMatrix.OfArray( array2D [[0.0; 0.0]; [0.0; 0.0]]) 
    ()

  // Test rotaci tensoru zapsanych jako matice a jako vektor
  let test02 () =
    // Bod do ktereho smeruje hlavni poloosa
    let x, y = 1.0, 0.8
    // Uhel od kladne poloosy x
    let theta = atan (y / x)
    // Theta ve stupnich
    let theta_deg = toDeg theta
    // Transormacni matice 3x3
    let T = getTransMatrix theta

    let eps_g_tens = [[1.0; 0.8; 0.0]; [0.8; 0.9; 0.0]; [0.0; 0.0; 0.3]] |> listToMatrix
    let eps_l_tens = eps_g_tens |> globalToLocal_tens T
    let eps_g_tens_check = eps_l_tens |> DenseMatrix.OfMatrix |> localToGlobal_tens T
    
    let eps_g_vec = eps_g_tens |> tensToVec
    let eps_l_vec = eps_g_vec |> globalToLocal_strain T
    let eps_g_vec_check = eps_l_vec |> localToGlobal_strain T

    printfn "T = %A" T
    printfn "T6 = %A" (getTransMatrix_6x6 T)
    
    printfn "eps_g_tens = %A" eps_g_tens
    printfn "eps_l_tens = %A" eps_l_tens
    printfn "eps_g_tens_check = %A" eps_g_tens_check
    printfn "----"
    printfn "eps_g_vec = %A" eps_g_vec
    printfn "eps_l_vec = %A" eps_l_vec
    printfn "eps_g_vec_check = %A" eps_g_vec_check
    
    ()

  let test03() =
    // Bod kam smeruje hlavni poloosa
    let x, y = 1.0, 0.8
    // Uhel od kladne poloosy x
    let theta = atan (y / x)
    // Transormacni matice 3x3
    let T = getTransMatrix theta
    // Nakopirovano z mumechu
    let eps_l = [[-3.91881748e-001; 3.86932603e-001; 0.0]; [3.86932603e-001; -1.47305054e-001; 0.0]; [0.0; 0.0; 3.76176839e-001]] |> listToMatrix

    let e1 = [-0.101834; -0.00144743; 0.0; 0.170388; 0.0; 0.0] |> listToVec // Rotated strain (local)
    let e2 = [-0.228893; 0.125611; 0.0; -0.0115676; 0.0; 0.0] |> listToVec   // Rotated inclusion (global)

    let eg = e1 |> localToGlobal_strain T
    
    //printfn "eps_g = %A" eps_g
    printfn "e1 = %A" e1
    printfn "e2 = %A" e2
    printfn "eg = %A" eg

    // Node 5718
    let x_g = [0.421920885999; 0.788437457315; 0.0] |> listToVec
    let x_l = x_g |> globalToLocal_vec T
    printfn "x_g = %A" x_g
    printfn "x_l = %A" x_l

    ()