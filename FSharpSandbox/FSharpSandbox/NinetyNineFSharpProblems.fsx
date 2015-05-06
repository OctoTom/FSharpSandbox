// Problem formulations are taken from http://fssnip.net/an

/// (*) Problem 1 : Find the last element of a list.
/// Example in F#: 
/// > myLast [1; 2; 3; 4];;
/// val it : int = 4
/// > myLast ['x';'y';'z'];;
/// val it : char = 'z'
let rec myLast list =
  match list with
  | [] -> failwith "Empty list!"
  | h::[] -> h
  | h::t -> myLast t

let myLast2 list =
  list |> List.rev |> List.head

let myLast3 list =
  list |> List.reduce(fun _ x -> x)

myLast [1; 2; 3; 4]
myLast2 [1; 2; 3; 4]
myLast3 [1; 2; 3; 4]


/// (*) Problem 2 : Find the last but one element of a list.
/// (Note that the Lisp transcription of this problem is incorrect.) 
///
/// Example in F#: 
/// myButLast [1; 2; 3; 4];;
/// val it : int = 3
/// > myButLast ['a'..'z'];;
 /// val it : char = 'y'
let myLastButOne list =
  match list |> List.rev with
  | f::s::t -> s
  | f::[] -> failwith "Only one elemetn in the list."
  | [] -> failwith "Empty list."
let myLastButOne2 list =
  list |> List.rev |> List.tail |> List.head

let res1 = myLastButOne2 [1; 2; 3; 4]
let res2 = myLastButOne2 [1]
let res3:int = myLastButOne2 []


/// (*) Problem 3 : Find the K'th element of a list. The first element in the list is number 1.
/// Example: 
/// * (element-at '(a b c d e) 3)
/// c
/// 
/// Example in F#: 
/// > elementAt [1; 2; 3] 2;;
/// val it : int = 2
/// > elementAt (List.ofSeq "fsharp") 5;;
/// val it : char = 'r'
let elementAt list index =
  List.nth list (index - 1)
let res1 = elementAt [1; 2; 3] 2
let res2 = elementAt (List.ofSeq "fsharp") 5

let rec elementAt2 list index =
  match list, index with
  | h::_, 1 -> h
  | _::t, i -> elementAt2 t (i-1) 
  | [], _ -> failwith "Empty list."
let res1 = elementAt2 [1; 2; 3] 2
let res2 = elementAt2 (List.ofSeq "fsharp") 5

/// (*) Problem 4 : Find the number of elements of a list.
/// Example in F#: 
/// 
/// > myLength [123; 456; 789];;
/// val it : int = 3
/// > myLength <| List.ofSeq "Hello, world!"
/// val it : int = 13 
let myLength list =
  List.length list
let res1 = myLength [123; 456; 789]
let res2 = myLength <| List.ofSeq "Hello, world!"

let myLength2 list =
  let rec helper acc list =
    match list with
    | [] -> acc
    | _::t -> helper (acc+1) t
  helper 0 list
let res1 = myLength2 [123; 456; 789]
let res2 = myLength2 <| List.ofSeq "Hello, world!"


/// (*) Problem 5 : Reverse a list.
/// Example in F#: 
///
/// > reverse <| List.ofSeq ("A man, a plan, a canal, panama!")
/// val it : char list =
///  ['!'; 'a'; 'm'; 'a'; 'n'; 'a'; 'p'; ' '; ','; 'l'; 'a'; 'n'; 'a'; 'c'; ' ';
///   'a'; ' '; ','; 'n'; 'a'; 'l'; 'p'; ' '; 'a'; ' '; ','; 'n'; 'a'; 'm'; ' ';
///   'A']
/// > reverse [1,2,3,4];;
/// val it : int list = [4; 3; 2; 1]
let reverse list =
  let rec helper oldList newList =
    match oldList with
    | [] -> newList
    | h::t -> helper t (h::newList)
  helper list []
let list = List.ofSeq ("A man, a plan, a canal, panama!")
let res1 = reverse list

/// (*) Problem 6 : Find out whether a list is a palindrome.
/// A palindrome can be read forward or backward; e.g. (x a m a x).
/// 
/// Example in F#: 
/// > isPalindrome [1;2;3];;
/// val it : bool = false
/// > isPalindrome <| List.ofSeq "madamimadam";;
/// val it : bool = true
/// > isPalindrome [1;2;4;8;16;8;4;2;1];;
/// val it : bool = true
let isPalindrome list =
  let reversedList = list |> List.rev
  List.forall2 (fun x y -> x = y) list reversedList
let res1 = isPalindrome [1;2;3]
let res2 = isPalindrome (List.ofSeq "madamimadam")
let res3 = isPalindrome [1;2;4;8;16;8;4;2;1]

let isPalindrome2 list = list = List.rev list
let res1 = isPalindrome2 [1;2;3]
let res2 = isPalindrome2 (List.ofSeq "madamimadam")
let res3 = isPalindrome2 [1;2;4;8;16;8;4;2;1]


/// (**) Problem 7 : Flatten a nested list structure.
/// Transform a list, possibly holding lists as elements into a `flat' list by replacing each 
/// list with its elements (recursively).
///  
/// Example: 
/// * (my-flatten '(a (b (c d) e)))
/// (A B C D E)
///  
/// Example in F#: 
/// 
type 'a NestedList = List of 'a NestedList list | Elem of 'a
///
/// > flatten (Elem 5);;
/// val it : int list = [5]
/// > flatten (List [Elem 1; List [Elem 2; List [Elem 3; Elem 4]; Elem 5]]);;
/// val it : int list = [1;2;3;4;5]
/// > flatten (List [] : int NestedList);;
/// val it : int list = []
let nestedList = (List [Elem 1; List [Elem 2; List [Elem 3; Elem 4]; Elem 5]])
// My solution
let flatten list =
  let rec helper acc list =
    match list with
    | Elem a -> acc @ [a]
    | List(h::t)  -> acc @ (helper acc h) @ (helper acc (List t))
    | List([]) -> acc
  helper [] list
let res = flatten nestedList


/// (**) Problem 8 : Eliminate consecutive duplicates of list elements.
/// If a list contains repeated elements they should be replaced with a single copy of the 
/// element. The order of the elements should not be changed.
///  
/// Example: 
/// * (compress '(a a a a b c c a a d e e e e))
/// (A B C A D E)
///  
/// Example in F#: 
/// 
/// > compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
/// val it : string list = ["a";"b";"c";"a";"d";"e"]
let compress list =
  list |> List.fold (fun acc item -> if (acc = [] || acc.Head <> item) then item::acc else acc ) [] |> List.rev

let res1 = compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
let res2 : int list  = compress [];;


/// (**) Problem 9 : Pack consecutive duplicates of list elements into sublists.
/// If a list contains repeated elements they should be placed 
/// in separate sublists.
///  
/// Example: 
/// * (pack '(a a a a b c c a a d e e e e))
/// ((A A A A) (B) (C C) (A A) (D) (E E E E))
///  
/// Example in F#: 
/// 
/// > pack ['a'; 'a'; 'a'; 'a'; 'b'; 'c'; 'c'; 'a'; 
///         'a'; 'd'; 'e'; 'e'; 'e'; 'e']
/// val it : char list list =
///  [['a'; 'a'; 'a'; 'a']; ['b']; ['c'; 'c']; ['a'; 'a']; ['d'];
///   ['e'; 'e'; 'e'; 'e']]
let pack list =
  let folder item acc =
    match acc with
    | (h::t)::tail when h = item -> (item::h::t)::tail
    | a -> [item]::a
  List.foldBack folder list []
let res1 = pack ['a'; 'a'; 'a'; 'a'; 'b'; 'c'; 'c'; 'a'; 'a'; 'd'; 'e'; 'e'; 'e'; 'e']
let res2 : (string list list) = pack []


/// (*) Problem 10 : Run-length encoding of a list.
/// Use the result of problem P09 to implement the so-called run-length 
/// encoding data compression method. Consecutive duplicates of elements 
/// are encoded as lists (N E) where N is the number of duplicates of the element E.
///  
/// Example: 
/// * (encode '(a a a a b c c a a d e e e e))
/// ((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))
///  
/// Example in F#: 
/// 
/// encode <| List.ofSeq "aaaabccaadeeee"
/// val it : (int * char) list =
///   [(4,'a');(1,'b');(2,'c');(2,'a');(1,'d');(4,'e')]
let encode list =
  list |> pack |> List.map (fun item -> (item.Length, item.Head)) // Function "pack" is defined in Problem 9.
let res1 = encode <| List.ofSeq "aaaabccaadeeee"


/// (*) Problem 11 : Modified run-length encoding.
/// Modify the result of problem 10 in such a way that if an element has no duplicates it 
/// is simply copied into the result list. Only elements with duplicates are transferred as
/// (N E) lists.
///  
/// Example: 
/// * (encode-modified '(a a a a b c c a a d e e e e))
/// ((4 A) B (2 C) (2 A) D (4 E))
///  
/// Example in F#: 
/// 
/// > encodeModified <| List.ofSeq "aaaabccaadeeee"
/// val it : char Encoding list =
///   [Multiple (4,'a'); Single 'b'; Multiple (2,'c'); Multiple (2,'a');
///    Single 'd'; Multiple (4,'e')]
type 'a Encoding = Multiple of int * 'a | Single of 'a
let encodeModified list =
  let mapping item =
    match item with
    | (1, a) -> Single(a)
    | a -> Multiple(a)
  list |> encode |> List.map mapping

let res1 = encodeModified <| List.ofSeq "aaaabccaadeeee"
let res2 = encodeModified <| List.ofSeq ""


/// (**) Problem 12 : Decode a run-length encoded list.
/// Given a run-length code list generated as specified in problem 11. Construct its 
/// uncompressed version.
///  
/// Example in F#: 
/// 
/// > decodeModified 
///     [Multiple (4,'a');Single 'b';Multiple (2,'c');
///      Multiple (2,'a');Single 'd';Multiple (4,'e')];;
/// val it : char list =
///   ['a'; 'a'; 'a'; 'a'; 'b'; 'c'; 'c'; 'a'; 'a'; 'd'; 'e'; 'e'; 'e'; 'e']
type 'a Encoding = Multiple of int * 'a | Single of 'a
let decodeModified xs =
  let mapping = function
    | Single(a) -> [a]
    | Multiple(num, a) -> List.replicate num a
  xs |> List.collect mapping

let res1 = decodeModified [Multiple (4,'a'); Single 'b'; Multiple (2,'c');  Multiple (2,'a'); Single 'd';Multiple (4,'e')]


/// (**) Problem 13 : Run-length encoding of a list (direct solution).
/// Implement the so-called run-length encoding data compression method directly. I.e. 
/// don't explicitly create the sublists containing the duplicates, as in problem 9, 
/// but only count them. As in problem P11, simplify the result list by replacing the 
/// singleton lists (1 X) by X.
///  
/// Example: 
/// * (encode-direct '(a a a a b c c a a d e e e e))
/// ((4 A) B (2 C) (2 A) D (4 E))
///  
/// Example in F#: 
/// 
/// > encodeDirect <| List.ofSeq "aaaabccaadeeee"
/// val it : char Encoding list =
///   [Multiple (4,'a'); Single 'b'; Multiple (2,'c'); Multiple (2,'a');
///    Single 'd'; Multiple (4,'e')]
type 'a Encoding = Multiple of int * 'a | Single of 'a
let encodeDirect xs =
  let backFolder item = function
    | Single a :: t when a = item -> Multiple (2, a) :: t
    | Multiple (num, a) :: t when a = item -> Multiple (num + 1, a) :: t
    | t -> Single item :: t 
  List.foldBack backFolder xs []

let res1 = encodeDirect <| List.ofSeq "aaaabccaadeeee"


/// (*) Problem 14 : Duplicate the elements of a list.
/// Example: 
/// * (dupli '(a b c c d))
/// (A A B B C C C C D D)
///  
/// Example in F#: 
/// 
/// > dupli [1; 2; 3]
/// [1;1;2;2;3;3]
let dupli1 xs = xs |> List.collect (fun item -> List.replicate 2 item)
let dupli2 xs = xs |> List.collect (List.replicate 2)
let dupli3 xs = [for x in xs do yield! [x; x]]

let res1 = dupli3 [1; 2; 3]


/// (**) Problem 15 : Replicate the elements of a list a given number of times.
/// Example: 
/// * (repli '(a b c) 3)
/// (A A A B B B C C C)
///  
/// Example in F#: 
/// 
/// > repli (List.ofSeq "abc") 3
/// val it : char list = ['a'; 'a'; 'a'; 'b'; 'b'; 'b'; 'c'; 'c'; 'c']
let repli1 xs num = xs |> List.collect (List.replicate num)
let repli2 xs num = [for x in xs do yield! [for y in 1..num -> x]]
let repli3 xs num = [for x in xs do for i = 1 to num do yield x] 
let res1 = repli1 (List.ofSeq "abc") 3
let res2 = repli2 (List.ofSeq "abc") 3
let res3 = repli3 (List.ofSeq "abc") 3


/// (**) Problem 16 : Drop every N'th element from a list.
/// Example: 
/// * (drop '(a b c d e f g h i k) 3)
/// (A B D E G H K)
///  
/// Example in F#: 
/// 
/// > dropEvery (List.ofSeq "abcdefghik") 3;;
/// val it : char list = ['a'; 'b'; 'd'; 'e'; 'g'; 'h'; 'k']
let dropEvery xs num =
  xs |> List.mapi (fun index item -> (item, (index+1) % num <> 0)) |> List.collect (fun (item, test) -> if test then [item] else [])

let res1 = dropEvery (List.ofSeq "abcdefghik") 3


/// (*) Problem 17 : Split a list into two parts; the length of the first part is given.
/// Do not use any predefined predicates. 
/// 
/// Example: 
/// * (split '(a b c d e f g h i k) 3)
/// ( (A B C) (D E F G H I K))
///   
/// Example in F#: 
/// 
/// > split (List.ofSeq "abcdefghik") 3
/// val it : char list * char list =
///   (['a'; 'b'; 'c'], ['d'; 'e'; 'f'; 'g'; 'h'; 'i'; 'k'])
let split xs length =
  let sqn = List.toSeq xs
  (sqn |> Seq.take length |> Seq.toList, sqn |> Seq.skip length |> Seq.toList)
// Later try to solve without predefined take and skip.

let res = split (List.ofSeq "abcdefghik") 3
 

/// (**) Problem 18 : Extract a slice from a list.
/// Given two indices, i and k, the slice is the list containing the elements between the 
/// i'th and k'th element of the original list (both limits included). Start counting the 
/// elements with 1.
///  
/// Example: 
/// * (slice '(a b c d e f g h i k) 3 7)
/// (C D E F G)
///  
/// Example in F#: 
/// 
/// > slice ['a';'b';'c';'d';'e';'f';'g';'h';'i';'k'] 3 7;;
/// val it : char list = ['c'; 'd'; 'e'; 'f'; 'g']
let slice xs fromIndex toIndex = // both indices are inclusive
  xs |> List.toSeq |> Seq.skip (fromIndex-1) |> Seq.take (toIndex-fromIndex+1) |> Seq.toList
let res = slice ['a';'b';'c';'d';'e';'f';'g';'h';'i';'k'] 3 2


/// (**) Problem 19 : Rotate a list N places to the left.
/// Hint: Use the predefined functions length and (@) 
/// 
/// Examples: 
/// * (rotate '(a b c d e f g h) 3)
/// (D E F G H A B C)
/// 
/// * (rotate '(a b c d e f g h) -2)
/// (G H A B C D E F)
///  
/// Examples in F#: 
/// 
/// > rotate ['a';'b';'c';'d';'e';'f';'g';'h'] 3;;
/// val it : char list = ['d'; 'e'; 'f'; 'g'; 'h'; 'a'; 'b'; 'c']
///  
/// > rotate ['a';'b';'c';'d';'e';'f';'g';'h'] (-2);;
/// val it : char list = ['g'; 'h'; 'a'; 'b'; 'c'; 'd'; 'e'; 'f']
let rotate xs offset =
  let offset =
    match offset % List.length xs with
    | a when a < 0 -> a + List.length xs
    | a -> a
  let sqn = List.toSeq xs
  (sqn |> Seq.skip offset |> Seq.toList) @ (sqn |> Seq.take offset |> Seq.toList)
let res1 = rotate ['a';'b';'c';'d';'e';'f';'g';'h'] 3
let res2 = rotate ['a';'b';'c';'d';'e';'f';'g';'h'] -2

let res = -1 % 3

/// (*) Problem 20 : Remove the K'th element from a list.
/// Example in Prolog: 
/// ?- remove_at(X,[a,b,c,d],2,R).
/// X = b
/// R = [a,c,d]
///  
/// Example in Lisp: 
/// * (remove-at '(a b c d) 2)
/// (A C D)
///  
/// (Note that this only returns the residue list, while the Prolog version also returns 
/// the deleted element.)
///  
/// Example in F#: 
/// 
/// > removeAt 1 <| List.ofSeq "abcd";;
/// val it : char * char list = ('b', ['a'; 'c'; 'd'])    
let removeAt num xs =
  (List.nth xs num,
    xs |> List.mapi (fun idx itm -> (idx, itm))
    |> List.filter (fun (idx, _) -> idx <> num)
    |> List.map snd)
let res = removeAt 1 <| List.ofSeq "abcd"


/// (*) Problem 21 : Insert an element at a given position into a list.
/// Example: 
/// * (insert-at 'alfa '(a b c d) 2)
/// (A ALFA B C D)
///  
/// Example in F#: 
/// 
/// > insertAt 'X' (List.ofSeq "abcd") 2;;
/// val it : char list = ['a'; 'X'; 'b'; 'c'; 'd']

/// Returns tuple of two lists. First list contains n first elements, second the rest.
let split xs n =
  let sqn = List.toSeq xs
  (sqn |> Seq.take n |> Seq.toList, sqn |> Seq.skip n |> Seq.toList)

let insertAt x xs i = 
  let splited = split xs (i)
  fst splited @ x :: snd splited
  
let res = insertAt 'X' (List.ofSeq "abcd") 2


/// (*) Problem 22 : Create a list containing all integers within a given range.
/// Example: 
/// * (range 4 9)
/// (4 5 6 7 8 9)
///  
/// Example in F#: 
/// 
/// > range 4 9;;
/// val it : int list = [4; 5; 6; 7; 8; 9]
let range a b =
  [a .. b]
let res = range 4 9


/// (**) Problem 23 : Extract a given number of randomly selected elements from a list.
/// Example: 
/// * (rnd-select '(a b c d e f g h) 3)
/// (E D A)
///  
/// Example in F#: 
/// 
/// > rnd_select "abcdefgh" 3;;
/// val it : seq<char> = seq ['e'; 'a'; 'h']

/// Returns tuple of two lists. First list contains n first elements, second the rest.
/// This solution is biased IMHO since the random generator can generate two identical numbers and then the lower number may be prefered.
let rnd_select xs n =
  let rndInt =
    let rnd = new System.Random() in seq {while true do yield rnd.Next()}
  xs |> Seq.zip rndInt |> Seq.sortBy fst |> Seq.take n |> Seq.map snd |> Seq.toList
let res = rnd_select "abcdefgh" 3

// Unbiased solution
let removeAt num xs =
  (List.nth xs num,
    xs |> List.mapi (fun idx itm -> (idx, itm))
    |> List.filter (fun (idx, _) -> idx <> num)
    |> List.map snd)
let rnd = System.Random()
let getRandomElements xs num =
  let rec helper acc list =
    match acc with
    | acc when List.length acc = num -> acc
    | acc ->
      let removed = removeAt (rnd.Next(List.length list)) list
      helper ((fst removed)::acc) (snd removed)
  helper [] xs
let res = getRandomElements (List.ofSeq "abcdefgh") 3


/// (*) Problem 24 : Lotto: Draw N different random numbers from the set 1..M.
/// Example: 
/// * (rnd-select 6 49)
/// (23 1 17 33 21 37)
///  
/// Example in F#: 
/// 
/// > diff_select 6 49;;
/// val it : int list = [27; 20; 22; 9; 15; 29]

// Using previous functions
let diff_select n m =
  getRandomElements [1..m] n
let res = diff_select 6 49


/// (*) Problem 25 : Generate a random permutation of the elements of a list.
/// Example: 
/// * (rnd-permu '(a b c d e f))
/// (B A D C E F)
///  
/// Example in F#: 
/// 
/// > rnd_permu <| List.ofSeq "abcdef";;
/// val it : char list = ['b'; 'c'; 'd'; 'f'; 'e'; 'a']
let rnd_permu xs = 
  getRandomElements xs (List.length xs)

let res = rnd_permu <| List.ofSeq "abcdef"


/// (**) Problem 26 : Generate the combinations of K distinct objects chosen from the N elements of a list.
/// In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that 
/// there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficients). For 
/// pure mathematicians, this result may be great. But we want to really generate all the 
/// possibilities in a list.
///  
/// Example: 
/// * (combinations 3 '(a b c d e f))
/// ((A B C) (A B D) (A B E) ... )
///  
/// Example in F#: 
/// 
/// > combinations 3 ['a' .. 'f'];;
/// val it : char list list =
///   [['a'; 'b'; 'c']; ['a'; 'b'; 'd']; ['a'; 'b'; 'e']; ['a'; 'b'; 'f'];
///    ['a'; 'c'; 'd']; ['a'; 'c'; 'e']; ['a'; 'c'; 'f']; ['a'; 'd'; 'e'];
///    ['a'; 'd'; 'f']; ['a'; 'e'; 'f']; ['b'; 'c'; 'd']; ['b'; 'c'; 'e'];
///    ['b'; 'c'; 'f']; ['b'; 'd'; 'e']; ['b'; 'd'; 'f']; ['b'; 'e'; 'f'];
///    ['c'; 'd'; 'e']; ['c'; 'd'; 'f']; ['c'; 'e'; 'f']; ['d'; 'e'; 'f']] 
let rec comb n list =
  match (n, list) with
  | (0, _) -> [[]]
  | (_, []) -> []
  | (n, h::t) ->
      let useX =  (comb (n-1) t) |> List.map (fun xs -> h::xs)
      let noX = comb n t
      useX @ noX
let res = comb 3 ['a' .. 'f']

let comb n l =
  match (n, l) with
  | (0, _) -> [[]]
  | (_, []) -> []
  | (n, h::t) ->
    let withHead = (comb (n-1) t) |> List.map (fun item -> h::item) // Appends all combinations of the rest element to the head element.
    let withoutHead = comb n t // Computes all the combinations without the head element.
    withHead @ withoutHead
let res = comb 2 ['a' .. 'd'] |> List.length

// Original solutions
let rec combinations n xs =
    match xs, n with
        | [],_ -> [[]]
        | xs, 1 -> [for x in xs do yield [x]]
        | x::xs, n -> 
            [for ys in combinations (n-1) xs do
                yield x::ys
             if List.length xs > n then
                yield! combinations n xs
             else
                yield xs]
let rec combinations' n xs =
    let rec tails = function
        | [] -> [[]]
        | _::ys as xs -> xs::tails ys
    match xs, n with
        | _, 0 -> [[]]
        | xs, n ->
            [ for tail in tails xs do
                match tail with
                    | [] -> ()
                    | y::xs' ->
                        for ys in combinations' (n - 1) xs' do
                            yield y::ys ]

