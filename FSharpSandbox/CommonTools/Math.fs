namespace CommonTools

module Math =
  open MathNet.Numerics.LinearAlgebra
  
  /// Converts degrees to radians.
  let degToRad x = System.Math.PI / 180.0 * x

  /// Converts radians to degrees.
  let radToDeg x = 180.0 / System.Math.PI * x

    /// Newton-Raphson solver
  let solveNewtonRaphson (f:Vector<float>->Vector<float>, J:Vector<float>->Matrix<float>, x0:Vector<float>, tol:float) =
    let newX x = x - (J x).Inverse() * f x
    let mutable i = 0
    let mutable x = x0
    let mutable y = f x
    while (y.Norm(2.0) > tol) do // MathNet.Numerics.LinearAlgebra.Vector<float>.Norm(float d) is defined as (sum(abs(this[i])^p))^(1/p).
      i <- i + 1
      x <- newX x
      y <- f x
      printfn "Step %A" i
      printfn "x = %A" x
      printfn "y = %A" y
    (x, y, i)

  // Integrates from t=0 to t=1
  // From Wikipedia: y'(t) = f(t, y(t))
  let eulerMethod df (x0, x1) y0 numSteps =  
    let h = (x1 - x0) / float numSteps
    let generator state =
      let i = fst state + 1
      let x = x0 + float i / float numSteps *(x1 - x0)
      let y = snd state + h * df x
      if i <= numSteps then Some((x, y), (i, y)) else None
    let state = (0, y0)
    Seq.unfold generator state

  // Updates initial state by update function until condition is met.
  // Returns None when max number of iteration is reached.
  let iterate (initialState, updateFunc, condition, maxIter) =
    // Condition that accepts option
    let optionCondition stateOption =
      match stateOption with
      | Some state -> condition state
      | None -> false
    let mutable i = 0
    let mutable state = Some initialState
    // Invariant: we have updated state i-times.
    while optionCondition state do
      i <- i + 1
      state <- if i < maxIter then Some (updateFunc state.Value) else None
    (state, i)

