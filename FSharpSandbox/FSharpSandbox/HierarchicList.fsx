type Tree<'a> =
  | Node of 'a * Tree<'a> list

let tree = Node ("This is root", [Node ("First node at level 1", []); Node ("Second node at level 1", [])])
let Node (a, b) = tree

let print tree =
  let rec print depth tree =
    let Node (value, list) = tree
    printfn "%A (Level %A)" value detph
    list |> List.iter (print (depth + 1))
  print 0 tree
  
