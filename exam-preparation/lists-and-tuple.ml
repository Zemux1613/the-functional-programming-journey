type student = {name: string ; age: int ; grade: float}

let rec student_over_age (age : int) (all : student list) : student list = match all with
| [] -> []
| x::xs -> if x.age > age then x :: student_over_age age xs else student_over_age age xs

(** ---------------------------------------------------------------------------------- **)

let rec sum_list (all : student list) : float = match all with
| [] -> 0.0
| x::xs -> x.grade +. sum_list xs

let average_grade (all : student list) : float = match all with
| [] -> 0.0
| _ -> sum_list all /. float_of_int (List.length all)
