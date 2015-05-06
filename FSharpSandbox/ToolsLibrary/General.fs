namespace ToolsLibrary
module General =
    // Unordered pair (set of two elements) with custom equality and hashing.
  // The expression FairPair (1, 2) = FairPair(2, 1) returns true.
  [<CustomEquality; NoComparison>]    
  type FairPair = // Single-case discriminated union
    | FairPair of (int * int)
    member x.Value = // Property "value" extracts the content of the union. (http://stackoverflow.com/questions/12232187/concise-pattern-match-on-single-case-discriminated-union-in-f)
      let (FairPair value) = x in value
    member x.Fst = // Property Fst returns first member of tuple.
      let (FairPair (v, _)) = x in v
    member x.Snd = // Property Snd returns second member of tuple.
      let (FairPair (_, v)) = x in v
    override x.Equals(yobj) =
      match yobj with
      | :? FairPair as y -> ((x.Fst = y.Fst && x.Snd = y.Snd) || (x.Fst = y.Snd && x.Snd = y.Fst))
      | _ -> false  
    override x.GetHashCode() = hash x.Fst + hash x.Snd
  // Results of comparison
  let res1 = FairPair (1, 2) = FairPair(1, 2)
  let res2 = FairPair (1, 2) = FairPair(2, 1)
  let res3 = FairPair (1, 1) = FairPair(1, 1)
  let res4 = FairPair (1, 1) = FairPair(1, 2)
  // Other funny stuf with single-case union:
  // Define three values of MyType type.
  let mt1 = FairPair (1, 2)
  // Bind the first of the typle to name "a".
  let (FairPair(a, _)) = mt1
  // Bind the second of the typle to name "b".
  let b = match mt1 with FairPair(_,x) -> x
  // Extract value using property Value.
  let tpl = mt1.Value

