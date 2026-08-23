(* ===== Assignment 5.2 Trie base definitions needed for Assignment 6.1 ===== *)

type ('k, 'v) trie = Trie of 'v option * ('k * ('k, 'v) trie) list

let empty = Trie (None, [])

let rec insert (Trie (rootv, ts) : ('k, 'v) trie) (key : 'k list) (value : 'v) : ('k, 'v) trie =
  match key with
  | [] -> Trie ((Some value), ts)
  | k :: ks -> let rec insertToChildList = function
    (* Trie does not contain subtrie with key, add new one *)
    | [] -> [k, insert empty ks value]
    | (subk, subt) :: ls -> if subk = k
      (* Found subtrie with correct key, insert the value with the tail of the key into it *)
      then (k, insert subt ks value) :: ls
      (* Subtrie has incorrect key, keep looking *)
      else (subk, subt) :: (insertToChildList ls)
    in
    Trie (rootv, (insertToChildList ts))

let rec remove (Trie (rootv, ts) : ('k, 'v) trie) (key : 'k list) : ('k, 'v) trie =
  match key with
  (* Key is empty, clear this trie *)
  | [] -> empty
  | k :: ks -> let rec removeFromChildList = function
    (* Key not in child list, no need to do anything *)
    | [] -> []
    | (subk, subt) :: ls -> if subk = k
      then
        (* Remove the tail of the key from the subtree *)
        let subt' = remove subt ks in
        (* Only keep the subtree if it's non-empty to avoid having useless nodes *)
        if subt' = empty then ls else (subk, subt') :: ls
      else (subk, subt) :: (removeFromChildList ls)
  in
  Trie (rootv, removeFromChildList ts)

(* Like lookup, but returns the matched trie instead of just its value *)
let rec lookupTrie (Trie (rootv, ts) : ('k, 'v) trie) (key : 'k list) : ('k, 'v) trie option =
  match key with
  (* Empty key yields this trie *)
  | [] -> Some (Trie (rootv, ts))
  (* Look for child with key and recurse on it, if present *)
  | k :: ks -> Option.bind
    (List.find_opt (fun (k', _) -> k' = k) ts)
    (fun (_, subt) -> lookupTrie subt ks)

let lookup (t : ('k, 'v) trie) (key : 'k list) : 'v option =
  Option.bind (lookupTrie t key) (fun (Trie (v, _)) -> v)

(* Returns a list of all values in the given trie *)
let rec allValues (Trie (v, ts) : ('k, 'v) trie) : ('v list) =
  let subvals = List.concat_map (fun (_, t) -> allValues t) ts in
  match v with
  | None -> subvals
  | Some v' -> v' :: subvals

let matches (t : ('k, 'v) trie) (prefix : 'k list) : 'v list =
  Option.value ~default:[] (Option.map allValues (lookupTrie t prefix))

(* ===== Assignment 6.1: Higher-order tree functions ===== *)

let rec trie_map (f : 'v -> 'w) (Trie (v, ch) : ('k, 'v) trie) : ('k, 'w) trie =
    Trie (Option.map f v, List.map (fun (k, t) -> k, trie_map f t) ch)

(*
    Given an option, either returns `None` if the option given is `None`, or, if the option is
    `Some x`, returns `Some x` iff the given predicate returns true, `None` otherwise.
*)
let filterOption (opt : 'a option) (f : ('a -> bool)) : 'a option =
    match opt with
    | None -> None
    | Some v -> if f v then Some v else None

let rec trie_filter (p : 'v -> bool) (Trie (v, children) : ('k, 'v) trie) : ('k, 'v) trie =
  let rec filterChildren = function
    | [] -> []
    | (k, c) :: cs ->
      let c' = trie_filter p c in
      if c' = empty then filterChildren cs else (k, c') :: filterChildren cs
  in
  Trie (filterOption v p, filterChildren children)

let rec trie_fold_left (f : 'acc -> 'v -> 'acc) (acc : 'acc) (Trie (v, children) : ('k, 'v) trie) : 'acc =
  let acc' = match v with
    | Some v' -> f acc v'
    | None -> acc
  in
  List.fold_left (fun acc'' (_, t) -> trie_fold_left f acc'' t) acc' children

let rec trie_fold_right (f : 'v -> 'acc -> 'acc) (Trie (v, children) : ('k, 'v) trie) (acc : 'acc) : 'acc =
  let acc' = List.fold_right (fun (_, t) acc' -> trie_fold_right f t acc') children acc in
  match v with
  | Some v' -> f v' acc'
  | None -> acc'

(* ===== Assignment 6.2: Proofs on programs ===== *)

(* length fn definition *)
let rec length = function
  | [] -> 0
  | _ :: xs -> 1 + length xs;;

(* @ definition *)
let rec (@) xs ys = match xs with
  | []      -> ys
  | x :: xs -> x :: (xs @ ys)  

(*
∀ xs, ys ∈ ’a list: (length xs) + (length ys) = length (xs @ ys)

Case xs = []
length [] + length ys
= 0 + length ys          (length [] = 0)
= length ys              (Arithmetik)
= length ([] @ ys)		 ([] -> ys, rückwärts)

Case xs = x :: xs'
length (x :: xs') + length ys 
= (1 + length xs') + length ys  (_ :: xs -> 1 + length xs)
= 1 + (length xs' + length ys)  (Arithmetik)
= 1 + length (xs' @ ys)         (Induktionshypothese)
= length (x :: (xs' @ ys))      (_ :: xs -> 1 + length xs, rückwärts)
= length ((x :: xs') @ ys)      (x :: (xs' @ ys) = (x :: xs') @ ys, rückwärts)
*)

(* ===== Assignment 6.3: OCaml modules - fractions ===== *)

module Fraction : sig
  type t

  val make : int -> int -> t
  val numerator : t -> int
  val denominator : t -> int
  val to_string : t -> string

  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
end = struct
  type t = int * int

  let gcd a b =
    let a = abs a and b = abs b in
    let rec aux a b = if b = 0 then a else aux b (a mod b) in
    aux a b

  let make (n : int) (d : int) : t =
    if d = 0 then failwith "Fraction.make: denominator must not be zero";
    let g = gcd (abs n) (abs d) in
    let sign = if d < 0 then -1 else 1 in
    (sign * n / g, sign * d / g)

  let numerator (f : t) : int = fst f

  let denominator (f : t) : int = snd f

  let to_string (f : t) : string =
    string_of_int (fst f) ^ "/" ^ string_of_int (snd f)

  let add (f1 : t) (f2 : t) : t =
    let (n1, d1) = f1 and (n2, d2) = f2 in
    make (n1 * d2 + n2 * d1) (d1 * d2)

  let sub (f1 : t) (f2 : t) : t =
    let (n1, d1) = f1 and (n2, d2) = f2 in
    make (n1 * d2 - n2 * d1) (d1 * d2)

  let mul (f1 : t) (f2 : t) : t =
    let (n1, d1) = f1 and (n2, d2) = f2 in
    make (n1 * n2) (d1 * d2)

  let div (f1 : t) (f2 : t) : t =
    let (n1, d1) = f1 and (n2, d2) = f2 in
    make (n1 * d2) (d1 * n2)

end

(* ===== Assignment 6.4: OCaml references ===== *)

let var_counter : int ref = ref 0

let freshVarName (prefix : string) : string =
  let ident = prefix ^ string_of_int !var_counter in
  incr var_counter; ident
