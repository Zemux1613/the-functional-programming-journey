type 'a tree = Empty | Node of 'a * 'a tree * 'a tree

(**
insert : int → int bst → int bst

Algorithmus:
- Wenn tree leer: erstelle neuen Node mit diesem Wert
- Wenn Wert kleiner als Node-Wert: insert rekursiv links
- Wenn Wert größer: insert rekursiv rechts
- Wenn gleich: tue nichts (oder ersetze)
**)
let rec insert (wert : int) (current : tree) : tree = match current with
| Empty -> tree(wert, Empty, Empty)
| Node(v, l , r) -> if v > wert Node(v, insert wert l, r)
                    else if v < wert Node(v, l, insert wert r)
                    else tree

(**
search : int → int bst → bool

Algorithmus:

Wenn tree leer: false
Wenn Wert gleich Node-Wert: true
Wenn kleiner: search rekursiv links
Wenn größer: search rekursiv rechts
**)
let rec search (wert : int) (current : tree) : bool = match current with 
| Empty -> false
| Node(v,l,r) -> if v = wert then true
                 else if v < wert then search wert l
                 else search wert r