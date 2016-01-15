namespace CommonTools

module List =
  /// Returns last element of the list. Or None, if the list is empty.
  let rec last list =
    match list with
    | [] -> None
    | h :: [] -> Some h
    | h :: t -> last t
