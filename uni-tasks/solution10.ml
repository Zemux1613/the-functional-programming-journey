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
  | Lambda      of string * expr
  | Application of expr * expr
  | Let         of bool * string * expr * expr (* first bool is true iff the let is recursive *)
  | If          of expr * expr * expr (* condition/then/else *)


(* Assignment 10.1 *)
type env = (string * expr) list;;

(* Assignment 10.1a *)
let addEnv (name, term) (term, expr) (env : env) : env = (name, term) :: env

(* Assignment 10.1b *)
let rec rmEnv name (env : env) : env = match env with
| [] -> []
| (var, term) :: rest -> if var = name then rest else (var, term) :: rmEnv name rest

(* Assignment 10.1c *)
let rec lookupEnv name env : env option = match env with 
| [] -> None
| (var, term) :: rest -> if var = name then Some term else lookupEnv name rest

(* Assignment 10.1d *)
let showEnv env : string = let binding2string (var, term) = var ^ " -> " ^ term in "{ " ^ String.concat "; " (List.map binding2string env) ^ " }"

