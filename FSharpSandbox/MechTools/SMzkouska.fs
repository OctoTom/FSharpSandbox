module SMzkouska

  let d11 = 64.0 * 5.0 / 3.0
  let d22 = 4.0 / 3.0
  let d12 = -8.0
  let d1f = 640.0 * 5.0 / 3.0
  let d2f = -80.0
  
  let x1 = -10.0
  let x2 = 0.0

  let left1 = d11 * x1 + d12 * x2 + d1f
  let left2 = d12 * x1 + d22 * x2 + d2f

