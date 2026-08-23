type tree = Empty | Node of int * int * tree * tree (** Node(key, value, left, right) **)

(** Sucht einen Key und gibt den Value zurück **)
let rec find (key : int) (current : tree) : int option = match current with
| Empty -> None
| Node(k, v, left, right) -> if key = k then Some v 
                         else if key < k then find key left 
                         else find key right

(** Gibt alle Keys zurück, die den geforderten Value haben **)
let rec find_all (value : int) (current : tree) : int list = match current with
| Empty -> []
| Node(k,v,l,r) -> let left_results = find_all value l in
                   let right_results = find_all value r in 
                   if v = value then left_results @ [k] @ right_results else left_results @ right_results

(** Fügt einen Wert in den Suchbaum ein **)
let rec insert (key: int ) (value : int) (current : tree) : tree = match current with
| Empty -> Node(key, value, Empty, Empty)
| Node(k, v, l, r) -> if k = key then Node(k, value, l, r) 
                      else if key < k then Node(k, v, insert key value l, r)
                      else Node(k, v, l, insert key value r)

(** Aktualisiert einen Wert im Suchbaum **)
let rec update (key: int) (value : int) (current : tree) : tree = match current with
| Empty -> Empty
| Node(k, v, l, r) -> if k = key then Node(k, value, l, r) 
                      else if key < k then Node(k, v, insert key value l, r)
                      else Node(k, v, l, insert key value r)

(** Findet den kleinsten Knoten im Baum **)
let rec find_min (current : tree) : int * int = match current with
| Empty -> failwith "Empty tree"
| Node (k,v,Empty, _) -> (k, v)
| Node (k,v, l, _) -> find_min l 

(** Löscht den kleinsten Knoten und gibt den modifizierten Baum zurück **)
let rec delete_min (current : tree) : tree = match current with
| Empty -> Empty
| Node(k,v,Empty,r) -> r
| Node(k,v,l,r) -> Node(k,v,delete_min l, r)

(** Löscht einen Knoten mit dem gegebenen Schlüssel aus dem Suchbaum **)
let rec delete (key : int ) (current : tree) : tree = match current with
| Empty -> Empty
| Node(k,v,l,r) -> if k = key then match l, r with
                                   | (Empty, Empty) -> Empty
                                   | (l, Empty) -> l
                                   | (Empty, r) -> r
                                   | (l,r) -> let (min_k, min_v) = find_min r in 
                                              let new_r = delete_min r in Node(min_k, min_v, l, new_r)
                    else if key < k then Node(k,v,delete key l, r)
                    else Node(k,v,l, delete key r)