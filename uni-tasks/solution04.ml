(* ===== Assignment 4.1 ===== *)

(*
  Type: 'a list -> 'a

  A small re-implementation of the `hd` function from the stdlib because that's not allowed and it
  allows us to write `heads` nicer than by using pattern matching.
*)
let head = function
  | (x::_) -> x
  | [] -> raise (Invalid_argument "Attempt to get head of empty list")

(*
  Type: 'a list list -> 'a list

  This cannot be written in point-free notation because it yields a curious typing error about types
  being non-generalizable.  Some searching around didn't yield any results about why this happens
  here.
*)
let heads l = List.map head l

(*
  Type: 'a list -> 'a list -> bool

  Checks if the first list is a prefix of the second
*)
let rec isPrefixOf x y = match x, y with
  | [], _ -> true
  | _, [] -> false
  | x::xs, y::ys when x == y -> isPrefixOf xs ys
  | _ -> false

(*
  Type: 'a list -> 'a list -> bool
*)
let rec isSublist needle haystack = match haystack with
  | [] -> false
  | _ when isPrefixOf needle haystack -> true
  | _::hs -> isSublist needle hs

(*
  Type: 'a list list -> 'a list list
*)
let revrev l = List.rev (List.map List.rev l)

(*
  Type: string -> char list

  Converts a string into a list of characters in original order.
*)
let rec string_to_chars s = match s with
  | "" -> []
  | _  -> String.get s 0 :: string_to_chars (String.sub s 1 (String.length s - 1))

(*
  Type: char list -> string

  Converts a list of characters back into a string.
*)
let rec chars_to_string l = match l with
  | []     -> ""
  | h :: t -> String.make 1 h ^ chars_to_string t 

(*
  Type: char -> char list -> bool

  Checks whether the character c appears in the delimiter list delims.
*)
let rec is_delim c delims = match delims with
  | []     -> false
  | h :: t -> if h = c then true else is_delim c t

(*
  Type: string -> string -> string list

  Tokenises the string s using the non-empty delimiter string d.
  Works by converting both s and d to char lists, then traversing s
  character by character, accumulating non-delimiter characters in a
  buffer and emitting a token whenever a delimiter is encountered.
*)
let tokenize d s =
  let chars = string_to_chars s in
  let delims = string_to_chars d in
  (*
    Type: char list -> char list -> string list

    Traverses chars and accumulates non-delimiter characters in buffer.
    Emits a token by converting buffer to a string whenever a delimiter is found.
  *)
  let rec aux chars buffer = match chars with
    | [] -> [chars_to_string (List.rev buffer)]
    | h :: t -> if is_delim h delims 
                then chars_to_string (List.rev buffer) :: aux t []
                else aux t (h :: buffer)
  in 
  let tokens = aux chars [] in
  List.filter (fun s -> s <> "") tokens

(*
  Type: 'a list -> 'a list * 'a list
  Splits a list into two halves by alternating elements.
*)
let rec split l = match l with
  | [] -> ([], [])
  | [x] -> ([x], [])
  | h1 :: h2 :: t -> let (l1, l2) = split t in (h1 :: l1, h2 :: l2)

(*
  Type: 'a list -> 'a list -> 'a list
  Merges two sorted lists into one sorted list.
*)
let rec merge l1 l2 = match (l1, l2) with
  | ([], _) -> l2
  | (_, []) -> l1
  | (h1 :: t1, h2 :: t2) -> if h1 <= h2 then h1 :: merge t1 l2 else h2 :: merge l1 t2

(*
  Type: 'a list -> 'a list
  Sorts a list in ascending order using the merge sort algorithm.
*)
let rec mergeSort l = match l with
  | []  -> []
  | [x] -> [x]
  | _   -> let (l1, l2) = split l in merge (mergeSort l1) (mergeSort l2)

(* ===== Assignment 4.2 ===== *)

(*
  Type: ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a list
  Takes a function f, an initial accumulator a and a list.
  Applies f to the accumulator and each element, collecting
  all intermediate accumulator values in a list.
*)
let rec scan f a = function
  | [] -> []
  | x :: xs -> let acc = f a x in acc :: (scan f acc xs)

(*
  Type: int list -> int list
  Computes the prefix sums of an integer list.
*)
let prefix_sum = scan ( + ) 0

(* ===== Assignment 4.3 ===== *)

(*
  Type: int list -> int -> int
  Given a list of coin values l and an amount n, computes
  how many different combinations of coins sum up to n.
*)
let rec coinPermute coins amount =
  if amount = 0 then 1
  else if amount < 0 then 0
  else match coins with
  | [] -> 0
  | c :: rest -> coinPermute coins (amount - c) + coinPermute rest amount