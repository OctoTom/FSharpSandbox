// Simplifying symbolic expressions //
// Inspired by article http://www.codeproject.com/Articles/87294/Symbolic-Calculation-in-F

// These expressions involve only addition and subtraction operators and unary negation
[<StructuralEquality; StructuralComparison>]
type Expr =
  | Nul // Zero expression
  | Sym of string // Symbol
  | Neg of Expr // Unary minus
  | Add of Expr * Expr // Binary plus
  | Sub of Expr * Expr // Binary minus
  static member Simplify (x:Expr) =
    let res = 
      match x with
      // Transforming expressions to required shape, i.e. (-a) + (-b) + c + (-d)
      | Neg(Neg x) -> x
      | Neg (Add(x, y)) -> Add(Neg x, Neg y)
      | Sub(x, y) -> Add(x, Neg y)
      | Add(x, Add(y, z)) -> Add(Add(x, y), z)
      // Simplifying transrormations involving Nul expression
      | Neg(Nul) -> Nul
      | Add(x, Nul) -> x
      | Add(Nul, x) -> x
      // Simplification based on equality of operands
      | Add(Neg x, y) when x = y -> Nul
      | Add(x, Neg y) when x = y -> Nul
      | Add(Add(x, y), Neg z) when y = z -> x
      | Add(Add(x, Neg y), z) when y = z -> x
      // Sorting leaf Add expressions in such as a + (-b)
      | Add(Sym x, Sym y) when x > y -> Add(Sym y, Sym x)
      | Add(Neg(Sym x), Sym y) when x > y -> Add(Sym y, Neg(Sym x))
      | Add(Sym x, Neg(Sym y)) when x > y -> Add(Neg(Sym y), Sym x)
      | Add(Neg(Sym x), Neg(Sym y)) when x > y -> Add(Neg(Sym y), Neg(Sym x))
      // Sorting leaf Add Add expressions such as exp + (-a) + b
      | Add(Add(x, Sym y), Sym z) when y > z -> Add(Add(x, Sym z), Sym y)
      | Add(Add(x, Neg(Sym y)), Sym z) when y > z -> Add(Add(x, Sym z), Neg (Sym y))
      | Add(Add(x, Sym y), Neg(Sym z)) when y > z -> Add(Add(x, Neg(Sym z)), Sym y)
      | Add(Add(x, Neg (Sym y)), Neg(Sym z)) when y > z -> Add(Add(x, Neg(Sym z)), Neg(Sym y))
      // If it's not possible to simplify this expression simplify the subexpressions
      | Add(x, y) -> Add(Expr.Simplify x, Expr.Simplify y)
      | Neg x -> Neg (Expr.Simplify x)
      | _ -> x
    if res = x then res else Expr.Simplify res
  // Convertes left unbalanced tree of Add expressions to left unbalanced tree of Add or Sub expressions
  static member ToAddSubForm (x:Expr) =
    let res =
      match x with
      | Add (x, Neg y) -> Sub(Expr.ToAddSubForm x, y)
      | x -> x
    res

// Tests
let res01 = (Sub(Sub(Add(Neg(Sym "A"),Add(Sym "B", Sym "A")),Sub(Sym "B", Sym "A")),Sym "A") |> Expr.Simplify) = Nul
let res02 = (Sub(Neg(Neg(Sym "A")), Sym "A") |> Expr.Simplify) = Nul
let res03 = (Sub(Sym "A", Sym "A") |> Expr.Simplify) = Nul
let res04 = (Neg(Neg(Neg(Sym "A"))) |> Expr.Simplify) = Neg(Sym "A")
let res05 = (Add(Sym "B", Sym "A") |> Expr.Simplify) = Add(Sym "A", Sym "B")
let res06 = (Sub(Sym "B", Sym "A") |> Expr.Simplify) = Add(Neg(Sym "A"), Sym "B")
let res07 = (Sub(Neg (Sym "A"), Sym "A") |> Expr.Simplify) = Add(Neg(Sym "A"), Neg(Sym "A"))
let res08 = Sub(Sub(Neg (Sym "A"), Sym "A"), Neg(Sym "A")) |> Expr.Simplify
// (A+B)+(A-B)+(B-A)-A-B
let res09 = Sub(Sub(Add(Add(Add(Sym "A", Sym "B"), Sub(Sym "A", Sym "B")), Sub(Sym "B", Sym "A")), Sym "A"),Sym "B") |> Expr.Simplify
// (A+B)+(A-B)+(B-A)
let res10 = Add(Add(Add(Sym "A", Sym "B"), Sub(Sym "A", Sym "B")), Sub(Sym "B", Sym "A")) |> Expr.Simplify
