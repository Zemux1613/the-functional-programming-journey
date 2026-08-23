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

(* freshVarName from sheet 6 *)
let var_counter : int ref = ref 0
let freshVarName (prefix : string) : string =
  let ident = prefix ^ string_of_int !var_counter in
  incr var_counter; ident

let freevars expr =
  let rec freeWith bound = function
    (* Every time we find an ident, we add it to our list iff it isn't a known bound ident. *)
    | Id ident -> if List.mem ident bound then [] else [ident]

    (* When we encounter something that introduces a new binding, we save that in a list and
    recurse. *)
    | Lambda (bind, body) -> freeWith (bind :: bound) body
    | Let (_, bind, expr, body) -> List.sort_uniq compare
      (* Bind isn't bound yet in expr, but it is in body. *)
      (freeWith bound expr @ freeWith (bind :: bound) body)

    (* Boring nodes, only recurse with children and de-duplicate finds. *)
    | MonOp (_, x) -> freeWith bound x
    | BinOp (_, l, r)
    | Application (l, r) -> List.sort_uniq compare (freeWith bound l @ freeWith bound r)
    | Const _ -> []
    | If (cond, thenv, elsev) ->
        List.sort_uniq compare (freeWith bound cond @ freeWith bound thenv @ freeWith bound elsev)
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
    | If (cond, thenv, elsev) ->
      If (subst cond source target, subst thenv source target, subst elsev source target)

    (* not-so-boring cases *)
    | Lambda (param, body) ->
      let (param', body') = (substWithNew param body) in
      Lambda (param', body')
    | Let (recursive, newvar, rhs, body) ->
      let (newvar', body') = substWithNew newvar body in
      Let (recursive, newvar', subst rhs source target, body')

let eval_mon_op op c = match op, c with
  | Neg, IntConst i -> IntConst (-i)
  | Not, BoolConst b -> BoolConst (not b)
  | _ -> failwith "Invalid unary operation"

let eval_bin_op op c1 c2 = match op, c1, c2 with
  | ArithOp Add, IntConst a, IntConst b -> IntConst (a + b)
  | ArithOp Sub, IntConst a, IntConst b -> IntConst (a - b)
  | ArithOp Mul, IntConst a, IntConst b -> IntConst (a * b)
  | ArithOp Div, IntConst a, IntConst b -> IntConst (a / b)
  | ArithOp Mod, IntConst a, IntConst b -> IntConst (a mod b)
  | RelOp Eq,  IntConst a, IntConst b -> BoolConst (a = b)
  | RelOp Neq, IntConst a, IntConst b -> BoolConst (a <> b)
  | RelOp Lt,  IntConst a, IntConst b -> BoolConst (a < b)
  | RelOp Leq, IntConst a, IntConst b -> BoolConst (a <= b)
  | RelOp Gt,  IntConst a, IntConst b -> BoolConst (a > b)
  | RelOp Geq, IntConst a, IntConst b -> BoolConst (a >= b)
  | RelOp Eq,  BoolConst a, BoolConst b -> BoolConst (a = b)
  | RelOp Neq, BoolConst a, BoolConst b -> BoolConst (a <> b)
  | LogicOp And, BoolConst a, BoolConst b -> BoolConst (a && b)
  | LogicOp Or,  BoolConst a, BoolConst b -> BoolConst (a || b)
  | _ -> failwith "Invalid binary operation"

(*
  Eager reduction for recursive functions doesn't work in most cases.  The reason is that, when we
  evaluate a function application, we first evaluate the function until we get a lambda and then
  insert the argument.  We also need the function to be a lambda, as otherwise we cannot substitute
  in the argument since we don't know what it's called.  Recursive functions, however generally
  cannot be reduced to a lambda without the value of their argument being known.  Consider, for
  example, our fibonnaci example:
    let rec fib n = if n <= 0 then 1 else n * fib (n - 1) in fib 4
  The `if` would generally allow this recursion to terminate if we're able to evaluate the condition
  `n <= 0` while expanding this definition to a Lambda.  However, since we have to evaluate the
  definition of `fib` before we can substitute in `n`, we perform reduction for a general variable
  `n`, not some concrete value, say `4`, preventing the reduction from ever terminating.

  Traditional imperative languages seem to get around this by functions not being higher-order,
  meaning that a function call would only ever refer to a function by name instead of a function
  being a value.  Since the name can be looked up, we don't get this issue.  Notice that, in, say,
  Java, you cannot declare a recursive lambda.  This is probably why.
*)
let rec reduce_eager expr = match expr with
  | Const _ -> expr
  | Id _ -> expr
  | MonOp (op, e) ->
    let e' = reduce_eager e in
    begin match e' with
    | Const c -> Const (eval_mon_op op c)
    | _ -> MonOp (op, e')
    end
  | BinOp (op, left, right) ->
    let left' = reduce_eager left in
    let right' = reduce_eager right in
    begin match left', right' with
    | Const c1, Const c2 -> Const (eval_bin_op op c1 c2)
    | _ -> BinOp (op, left', right')
    end
  | Lambda (param, body) -> Lambda (param, reduce_eager body)
  | Application (f, arg) -> 
    let f' = reduce_eager f in
    let arg' = reduce_eager arg in
    begin match f' with
    | Lambda (param, body) -> reduce_eager (subst body param arg')
    | _ -> Application (f', arg')
    end
  | Let (recursive, id, expr, body) ->
    let expr' = if recursive then eager_reduce_recursive_let id expr else reduce_eager expr in
    reduce_eager (subst body id expr')
  | If (cond, thenv, elsev) ->
    begin match reduce_eager cond with
    | Const (BoolConst b) ->
      (* This is, of course, not technically eager reduction.  As discussed during the lecture,
        entirely eager reduction never terminates with recursive functions, hence why we don't do
        that here. *)
      reduce_eager (if b then thenv else elsev)
    | cond' -> If (cond', thenv, elsev)
    end

(*
  Keeps substituting the value of a recursive let binding into itself until it isn't recursive
  anymore.
*)
and eager_reduce_recursive_let id body =
  let body' = reduce_eager body in
  let body'' = subst body' id body' in
  (* If we changed the body, keep recursing, otherwise we've reached a fixed point. *)
  if body' <> body'' then eager_reduce_recursive_let id body'' else body''

let rec reduce_normal_once expr =
  match expr with
  | Let (recursive, id, expr, body) ->
    (*
      This uses a neat transformation I came up with to correctly perform one step of a recursive
      let-binding:
      
      let rec x = f x in g x
      ⁻> g (let rec x = f x in x)      Pull let-rec into body for lazy argument evaluation
      ⁻> g (f (let rec x = f x in x))  Evaluate one step if we're in "reduced form"
    *)
    if recursive 
      then let reduced = Let (true, id, expr, Id id) in if body = Id id
        then (* in reduced form, perform one recursion step *)
          let body' = subst expr id reduced in
          if body' <> body then Some body' else None
        else Some (subst body id reduced)
      else Some (subst body id expr)
  | If (Const (BoolConst b), thenv, elsev) -> Some (if b then thenv else elsev)
  | If (cond, thenv, elsev) ->
    begin match reduce_normal_once cond with
    | Some c' -> Some (If (c', thenv, elsev))
    | None -> None
    end
  | Application (Lambda (param, body), arg) -> Some (subst body param arg)
  | Application (f, arg) ->
    begin match reduce_normal_once f with
      | Some f' -> Some (Application (f', arg))
      | None ->
        begin match reduce_normal_once arg with
        | Some arg' -> Some (Application (f, arg'))
        | None -> None
        end
      end
  | Lambda (param, body) ->
    begin match reduce_normal_once body with
    | Some body' -> Some (Lambda (param, body'))
    | None -> None
    end
  | MonOp (op, Const c) -> Some (Const (eval_mon_op op c))
  | MonOp (op, e) -> begin match reduce_normal_once e with
    | Some e' -> Some (MonOp (op, e'))
    | None -> None
    end
  | BinOp (op, Const c1, Const c2) -> Some (Const (eval_bin_op op c1 c2))
  | BinOp (op, left, right) -> begin match reduce_normal_once left with
      | Some left' -> Some (BinOp (op, left', right))
      | None ->
        begin match reduce_normal_once right with
        | Some right' -> Some (BinOp (op, left, right'))
        | None -> None
        end
      end
  | Const _ -> None
  | Id _ -> None

let rec reduce_normal expr =
  match reduce_normal_once expr with
  | Some expr' -> reduce_normal expr'
  | None -> expr
