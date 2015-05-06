// Symbolic expression
// Inspired by article http://www.codeproject.com/Articles/87294/Symbolic-Calculation-in-F
//[<CustomEquality; NoComparison>]
//[<CustomEquality; CustomComparison>]
[<StructuralEquality; StructuralComparison>]
type Expr =
  | Symbol of string
  | Const of float
  | Neg of Expr
  | Add of Expr * Expr
  | Sub of Expr * Expr
  | Mul of Expr * Expr
  | Div of Expr * Expr
  static member Eq (x:Expr) (y:Expr) =
    match x, y with
    | Symbol x, Symbol y -> x = y // X = X
    | Const x, Const y -> x = y // 42 = 42
    | Neg x, Neg y -> x = y // -ex = -ex
    | x, Neg(Neg y) -> x = y // ex = -(-ex)
    | Neg(Neg x), y -> x = y // -(-ex) = ex
    | Add(x1, x2), Add(y1, y2) -> x1 = y1 && x2 = y2 || x1 = y2 && x2 = y1 // ex1 + ex2 = ex1 + ex2 OR ex1 + ex2 = ex2 + ex1 Commutative operator
    | Sub(x1, x2), Sub(y1, y2) -> x1 = y1 && x2 = y2
    | Mul(x1, x2), Mul(y1, y2) -> x1 = y1 && x2 = y2 || x1 = y2 && x2 = y1 // Commutative operator
    | Div(x1, x2), Div(y1, y2) -> x1 = y1 && x2 = y2
    | _ -> false
//  override x.Equals(yObj) =
//    match yObj with
//    | :? Expr as y -> Expr.Eq x y
//    | _ -> false  
  static member Simplify (x:Expr) =
    match x with
    | Neg(Const c) -> Const (-c)
    | Neg(Neg(x)) -> x |> Expr.Simplify
    | Neg(Sub(x, y)) -> Sub(Expr.Simplify y ,Expr.Simplify x) |> Expr.Simplify
    | Add(x, y) when x < y -> Add(y |> Expr.Simplify, x |> Expr.Simplify) |> Expr.Simplify
    | Add(Const x, Const y) -> Const (x + y)
    | Add(Const 0.0, y) -> y |> Expr.Simplify
    | Add(y, Const 0.0) -> y |> Expr.Simplify
    | Add(x, Neg(y)) -> Sub(Expr.Simplify x, Expr.Simplify y) |> Expr.Simplify
    | Add(Neg(x), y) -> Sub(Expr.Simplify y, Expr.Simplify x) |> Expr.Simplify
    | Add(x, y) when x = y -> Mul(Const 2.0, x |> Expr.Simplify) |> Expr.Simplify
    | Add(Mul(Const c1, x), Mul(Const c2, y)) when x = y -> Mul(Const(c1 + c2), Expr.Simplify x) |> Expr.Simplify
    | Sub(x, y) when x = y -> Const 0.0
    | Sub(Const x, Const y) -> Const (x - y)
    | Sub(Const 0.0, y) -> Neg y |> Expr.Simplify
    | Sub(y, Const 0.0) -> y |> Expr.Simplify
    | Sub(x, y) when x = y -> Const 0.0
    | Mul(x, y) when x > y -> Add(y, x) |> Expr.Simplify
    | Mul(Const x, Const y) -> Const (x * y)
    | Mul(Const c, Neg(x)) -> Mul(Const (-c), x)
    | Mul(Neg(x), Const c) -> Mul(Const (-c), x)
    | Div(x, y) when x = y -> Const 1.0
    | Div(Const x, Const y) -> Const (x / y)
    | _ -> x

// Function returning opperator's name
let OpName (e: Expr) : string =
    match e with
    | Add(_, _) -> "+"
    | Sub(_, _) -> "-"
    | Mul(_, _) -> "*"
    | Div(_, _) -> "/"
    | _ -> failwith(sprintf "Unrecognized operator [%A]" e)
// Active pattern matching opperators
let (|Op|_|) (x : Expr) =
  match x with
  | Add(e1, e2) -> Some(Add, e1, e2)
  | Sub(e1, e2) -> Some(Sub, e1, e2)
  | Mul(e1, e2) -> Some(Mul, e1, e2)
  | Div(e1, e2) -> Some(Div, e1, e2)
  | _ -> None
// Function producing string representation of the expression.
let FormatExpression (x : Expr) : string =
  let rec FormatSubExpression (x : Expr, isOuter : bool) : string =
    match x with
    | Symbol name -> name;
    | Const n -> sprintf "%g" n
    | Neg x -> sprintf "-%s" (FormatSubExpression(x, false))
    | Op(op, e1, e2) ->
        let s = FormatSubExpression(e1, false) + " " + OpName(x) + " " + FormatSubExpression(e2, false)
        if isOuter then s else "(" + s + ")"
    | _ -> failwith(sprintf "Nothing matched.")
  FormatSubExpression(x, true)

let exp01 = Add(Add(Symbol "b", Symbol "c"), Symbol "a")
let res01 = exp01 |> Expr.Simplify
