let rec string_reverse (input : string) : string = 
if String.length input == 0: 
 ""
else 
 let first = String.get input 0 in 
 let rest = String.sub input 1 (String.length input - 1) in
 string_reverse rest ^ String.make 1 first

(** ---------------------------------------------------------------------------------- **)

let is_palindrome (input : string) : bool =
  let rec check left right =
    if left >= right then
      true
    else if input.[left] <> input.[right] then
      false
    else
      check (left + 1) (right - 1)
  in
  check 0 (String.length input - 1)

