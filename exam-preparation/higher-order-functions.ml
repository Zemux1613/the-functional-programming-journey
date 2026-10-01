let rec map_list (f : 'a -> 'b) (lst : 'a list) : 'b list = match lst with
| [] -> []
| x::xs -> f x :: map_list f xs

(** ---------------------------------------------------------------------------------- **)

let rec filter_list (f : 'a -> bool) (lst : 'a list) : 'a list = match lst with
| [] -> []
| x::xs -> if f x then x :: filter_list f xs else filter_list f xs