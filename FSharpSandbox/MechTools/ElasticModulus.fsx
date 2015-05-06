// Taken from Wikipedia http://en.wikipedia.org/wiki/Hooke%27s_law
let K E nu = E / 3.0 / (1.0 - 2.0 + nu)
let G E nu = E  / 2.0 / (1.0 + nu)

let E = 34000.0
let nu = 0.2

let res = G E nu
