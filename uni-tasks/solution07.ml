(* ===== 7.3: Lambda Calculus types ===== *)

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
  | Const       of const
  | Id          of string
  | BinOp       of bin_op * expr * expr
  | MonOp       of mon_op * expr

  (* == Newly added variants: == *)
  | Lambda      of string * expr
  | Application of expr * expr

  (* This is a simple version of let that introduces exactly one new variable.  We don't support
  multiple variables in one `let` and *definitely* don't support multiple declarations referencing
  eachother recursively.  I already implemented type resolution for that once in the compiler
  construction course and that's enough :P *)
  | Let         of string * expr * expr
  (* No need for a new "variable" variant, that's already what `Id` is for. *)

(* ===== 7.4: Lambda Calculus Functions ===== *)

(* ==== Helpers for converting other AST nodes to strings ==== *)
let const2string = function
  | IntConst i -> string_of_int i
  | BoolConst b -> if b then "true" else "false"

let bin_op2string = function
  | ArithOp a -> (match a with
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Div -> "/"
    | Mod -> "%")
  | RelOp r -> (match r with
    | Eq -> "=="
    | Neq -> "!="
    | Lt -> "<"
    | Leq -> "<="
    | Gt -> ">"
    | Geq -> ">=")
  | LogicOp l -> (match l with
    | And -> "&&"
    | Or -> "||")

let mon_op2string = function
  | Neg -> "-"
  | Not -> "!"

(* freshVarName from sheet 6 *)
let var_counter : int ref = ref 0
let freshVarName (prefix : string) : string =
  let ident = prefix ^ string_of_int !var_counter in
  incr var_counter; ident

(* ==== Functions from task ==== *)
let rec expr2string = function
  | Const c -> const2string c
  | Id i -> i
  (* This implementation may lead to missing parantheses where those would be needed to preserve
  associativity, but I've decided that the proper implementation of that is out of scope and the
  lazy implementation that always adds parantheses clutters the output too much, so this is fine. *)
  | BinOp (op, a, b) -> expr2string a ^ bin_op2string op ^ expr2string b
  | MonOp (op, x) -> mon_op2string op ^ expr2string x
  | Lambda (id, expr) -> "λ" ^ id ^ ".(" ^ expr2string expr ^ ")"
  | Application (f, x) -> "((" ^ expr2string f ^ ") (" ^ expr2string x ^ "))"
  | Let (ident, v, body) -> "let " ^ ident ^ " = " ^ expr2string v ^ " in " ^ expr2string body

let freevars expr =
  let rec freeWith bound = function
    (* Every time we find an ident, we add it to our list iff it isn't a known bound ident. *)
    | Id ident -> if List.mem ident bound then [] else [ident]

    (* When we encounter something that introduces a new binding, we save that in a list and
    recurse. *)
    | Lambda (bind, body) -> freeWith (bind :: bound) body
    | Let (bind, expr, body) -> List.sort_uniq compare
      (* Bind isn't bound yet in expr, but it is in body. *)
      (freeWith bound expr @ freeWith (bind :: bound) body)

    (* Boring nodes, only recurse with children and de-duplicate finds. *)
    | MonOp (_, x) -> freeWith bound x
    | BinOp (_, l, r)
    | Application (l, r) -> List.sort_uniq compare (freeWith bound l @ freeWith bound r)
    | Const _ -> []
  in
  freeWith [] expr

let rec subst expr source target =
  let substWithNew newvar body =
    let free = freevars body in
    if newvar = source then (newvar, body) (* parameter is bound within, do not recurse *)
    else if not (List.mem source free) then (newvar, subst body source target) (* no renaming needed *)
    else
      let fresh = freshVarName "__parasite" in
      let renamed = subst body newvar (Id fresh) in
      (fresh, subst renamed source target)
  in
  match expr with
    (* boring cases *)
    | Const _ -> expr
    | Id id -> if id = source then target else Id id
    | BinOp (op, l, r) -> BinOp (op, subst l source target, subst r source target)
    | MonOp (op, x) -> MonOp (op, subst x source target)
    | Application (f, x) -> Application (subst f source target, subst x source target)

    (* not-so-boring cases *)
    | Lambda (param, body) ->
      let (param', body') = (substWithNew param body) in
      Lambda (param', body')
    | Let (newvar, rhs, body) ->
      let (newvar', body') = substWithNew newvar body in
      Let (newvar', subst rhs source target, body')
