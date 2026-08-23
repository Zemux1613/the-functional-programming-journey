(* ===== 5.1: Algebraic Data Types for Expressions ===== *)

type arith_op = Add | Sub | Mul | Div | Mod

type rel_op = Eq | Neq | Lt | Leq | Gt | Geq

type logic_op = And | Or

type bin_op =
  | ArithOp of arith_op
  | RelOp   of rel_op
  | LogicOp of logic_op

type mon_op = Neg | Not

type const =
  | IntConst  of int
  | BoolConst of bool

type expr =
  | Const  of const
  | Id     of string
  | BinOp  of bin_op * expr * expr
  | MonOp  of mon_op * expr

(* ===== 5.2: Trie ===== *)

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

(*
  The task here is somewhat ambiguous: should all values where the key has the given key as a
  prefix be removed, or only the one value (if any) that has the given key as key exactly?
  We implement the former.
*)
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
