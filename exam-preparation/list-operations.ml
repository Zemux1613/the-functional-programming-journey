let rec sum_list (nums : int list) : int = match nums with 
| [] -> 0
| x::xs -> x + sum_list xs

(** ---------------------------------------------------------------------------------- **)

let rec product_list (nums : int list) : int = match nums with
| [] -> 1
| x::xs -> x * product_list xs

(** ---------------------------------------------------------------------------------- **)

let rec length (elements : 'a list) : int = match elements with
| [] -> 0
| x::xs -> 1 + length xs

(** ---------------------------------------------------------------------------------- **)

let rec reverse (elements : 'a list) : 'a list = match elements with
| [] -> []
| x::xs -> reverse xs @ [x]