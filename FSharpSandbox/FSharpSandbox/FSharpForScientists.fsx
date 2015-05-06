// Code snippets from F# for Scientists by Jon Harrop.

// Trees, p. 93 //

// Just tree without any additional info at nodes or leafs.
type tree = Node of tree list
// Function that creates balanced binary tree.
let rec balanced_tree = function
  | 0 -> Node []
  | n -> Node [balanced_tree (n-1); balanced_tree (n-1)]
// Balanced tree instance
let myTree = balanced_tree 3
// Function that counts leafs.
let rec leaf_count tree =
  match tree with
  | Node [] -> 1 // Leaf
  | Node list -> list |> List.sumBy (leaf_count) // Number of leafs in child trees.
// Compute the number of leafs.
leaf_count myTree

// Polymorphic tree type
type 'a ptree = PNode of 'a * 'a ptree list

let tr = Node [Node []; Node []]

let res =
  match tr with
  | Node list -> list |> List.length

// Function that creates polymorfic tree based on tree. The new tree contains zeros.
let rec zero_ptree_from_tree (Node root) =
  PNode (0, List.map zero_ptree_from_tree root)

// Instances of ptree containing zeros
zero_ptree_from_tree myTree
zero_ptree_from_tree tr

// Alternative zeroPTree function
let rec myZeroPTree root =
  match root with
  | Node subtrees -> PNode (0, subtrees |> List.map myZeroPTree)

