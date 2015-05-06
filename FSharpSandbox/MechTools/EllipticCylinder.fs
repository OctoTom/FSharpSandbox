module EllipticCylinder
  open System

  // Inclusion semiaxes
  let a = 0.2
  let b = 0.1

  // Poisson ratio
  let nu = 0.2

  let mult =  1.0 / 2.0 / (1.0 - nu)
  let S1111 = mult * ((b * b + 2.0 * a * b) / Math.Pow(a + b, 2.0) + (1.0 - 2.0 * nu) * b / (a + b))
  let S1122 = mult * (b * b / Math.Pow(a + b, 2.0) - (1.0 - 2.0 * nu) * b / (a + b))
  let S1133 = mult * 2.0 * nu * b / (a + b)
  let S2211 = mult * (a * a / Math.Pow(a + b, 2.0) - (1.0 - 2.0 * nu) * a / (a + b))
  let S2222 = mult * ((a * a + 2.0 * a * b) / Math.Pow(a + b, 2.0) + (1.0 - 2.0 * nu) * a / (a + b))
  let S2233 = mult * 2.0 * nu * a / (a + b)
  let S3311 = 0.0
  let S3322 = 0.0
  let S3333 = 0.0
  let S1212 = mult * ((a * a + b * b) / 2.0 / Math.Pow(a + b, 2.0) + (1.0 - 2.0 * nu) / 2.0)
  let S2323 = a / 2.0 / (a + b)
  let S3131 = b / 2.0 / (a + b)

