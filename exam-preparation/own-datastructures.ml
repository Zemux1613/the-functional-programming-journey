type expr = Num of int | Add of expr * expr | Sub of expr * expr | Mul of expr * expr

let rec eval (exp : expr) : int = match exp with
| Num(i) -> i
| Add(e1, e2) -> eval e1 + eval e2
| Sub(e1, e2) -> eval e1 - eval e2
| Mul(e1, e2) -> eval e1 * eval e2

let rec simplify (exp : expr) : expr = match exp with 
| Num(i) -> Num(i)
| Add(e1, e2) -> if e1 = Num(0) then simplify e2 else if e2 = Num(0) then simplify e1 else Add(simplify e1, simplify e2)
| Sub(e1, e2) -> if e2 = Num(0) then simplify e1 else Sub(simplify e1, simplify e2)
| Mul(e1, e2) -> if e1 = Num(1) then simplify e2 else if e2 = Num(1) then simplify e1 else Mul(simplify e1, simplify e2)

let rec to_string (exp : expr) : string = match exp with
| Num(i) -> string_of_int i
| Add(e1, e2) -> "(" ^ to_string e1 ^ "+" ^ to_string e2 ^ ")"
| Sub(e1, e2) -> "(" ^ to_string e1 ^ "-" ^ to_string e2 ^ ")"
| Mul(e1, e2) -> "(" ^ to_string e1 ^ "*" ^ to_string e2 ^ ")"