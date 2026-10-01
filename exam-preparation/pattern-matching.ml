type 'a tree = Empty | Node of 'a * 'a tree * 'a tree (** value, left, right **)

(** ---------------------------------------------------------------------------------- **)

let rec tree_sum (current : int tree) : int = match current with
| Empty -> 0
| Node(value, l, r) -> let ll = tree_sum l in
                       let rr = tree_sum r in
                       value + ll + rr

tree_sum (Node(1, Node(1, Empty, Empty), Node(1, Empty, Node(1, Empty, Empty))));;

(** ---------------------------------------------------------------------------------- **)

let rec tree_size (current : 'a tree) : int = match current with 
| Empty -> 0
| Node(value, l, r) -> 1 + tree_size l + tree_size r

(** ---------------------------------------------------------------------------------- **)

let rec tree_height (current : 'a tree) : int = match current with
| Empty -> 0
| Node(value, l, r) -> 1 + max (tree_height l) (tree_height r)