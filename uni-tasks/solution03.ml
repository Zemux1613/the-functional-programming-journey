open List

(* ===== Assignment 3.1 ===== *)

(*
  Type: (int, int) -> (int, int, string)
*)
let us_of_eu_time (h, m) =
  let h_us = match h mod 12 with | 0 -> 12 | n -> n in
  let phase = if h < 12 then "am" else "pm" in
  (h_us, m, phase)

(*
  Type: (int, int, string) -> (int, int)
*)
let eu_of_us_time (h, m, phase) =
  let
    h_eu = match h, phase with
    | (12, "pm") -> 12
    | (12, "am") -> 0
    | (n, "pm") -> n + 12
    | (n, "am") -> n
    | (_, garbage) -> raise (Invalid_argument ("Invalid time phase: " ^ garbage))
  in
  (h_eu, m)

(*
  Type: (int, int) -> (int, int) -> bool
*)
let laterthan_eu_time (h1, m1) (h2, m2) =
  h1 > h2 || (h1 = h2 && m1 > m2)

(*
  Type: (int, int, string) -> (int, int, string) -> bool
*)
let earlierthan_us_time (h1, m1, phase1) (h2, m2, phase2) = match phase1, phase2 with
  | ("am", "pm") -> true
  | ("pm", "am") -> false
  | _ ->
    let fixHour h = match h with | 12 -> 0 | n -> n in
    let h1 = fixHour h1 and h2 = fixHour h2 in
    h1 < h2 || (h1 = h2 && m1 < m2)

(*
  Type: int -> string
  Converts a minute value to a string that is always 2 digits in length.
*)
let string_of_minutes m =
  if m < 10 then "0" ^ string_of_int m else string_of_int m

(*
  Type: (int, int) -> string
*)
let string_of_eu_time (h, m) =
  string_of_int h ^ ":" ^ string_of_minutes m

(*
  Type: (int, int, string) -> string
*)
let string_of_us_time (h, m, phase) =
  string_of_int h ^ ":" ^ string_of_minutes m ^ phase

(* ===== Assignment 3.2 ===== *)

(*
  Type: bpd_imperial
  Reason: Record type with four int fields representing the old British
    imperial currency. 1 pound = 20 shillings, 1 shilling = 12 pence,
    1 penny = 4 farthings.
*)
type bpd_imperial = {
  pounds    : int;
  shillings : int;
  pence     : int;
  farthings : int;
}

(*
  Type: bpd_decimal
  Reason: Record type with two int fields representing the modern British
    decimal currency. 1 pound = 100 pence.
*)
type bpd_decimal = {
  pounds : int;
  pence  : int;
}

(*
  Type: bpd_imperial -> bpd_imperial
  Reason: Converts all fields to a single int (total farthings) using `+`
    and `*` on ints, then splits back using `/` and `mod` on ints.
    Input and output are both bpd_imperial records.
*)
let normalize_imperial (v : bpd_imperial) : bpd_imperial =
  let total =
    v.farthings
    + v.pence     * 4
    + v.shillings * 48
    + v.pounds    * 960
  in
  { pounds    = total / 960;
    shillings = (total / 48)  mod 20;
    pence     = (total / 4)   mod 12;
    farthings =  total        mod 4; }

(*
  Type: bpd_imperial -> bpd_imperial -> bpd_imperial
  Reason: Both operands are bpd_imperial records. Each field is added with
    `+` on ints, then passed to normalize_imperial to carry overflows.
    Returns a normalized bpd_imperial.
*)
let sum_imperial (a : bpd_imperial) (b : bpd_imperial) : bpd_imperial =
  normalize_imperial {
    pounds    = a.pounds    + b.pounds;
    shillings = a.shillings + b.shillings;
    pence     = a.pence     + b.pence;
    farthings = a.farthings + b.farthings;
  }

(*
  Type: bpd_imperial -> bpd_imperial -> bpd_imperial
  Reason: Both operands are bpd_imperial records. Each field is subtracted
    with `-` on ints, then passed to normalize_imperial which correctly
    handles negative intermediate values via total farthings. Returns a
    normalized bpd_imperial.
*)
let diff_imperial (a : bpd_imperial) (b : bpd_imperial) : bpd_imperial =
  normalize_imperial {
    pounds    = a.pounds    - b.pounds;
    shillings = a.shillings - b.shillings;
    pence     = a.pence     - b.pence;
    farthings = a.farthings - b.farthings;
  }

(*
  Type: bpd_decimal -> bpd_imperial
  Reason: Takes a bpd_decimal record and converts it to bpd_imperial.
    Uses the ratio 48/5 farthings per decimal penny (since 960 farthings
    = 100 decimal pence). Integer arithmetic with `*` and `/` on ints.
*)
let dec2imp (d : bpd_decimal) : bpd_imperial =
  let total = d.pounds * 960 + d.pence * 48 / 5 in
  normalize_imperial
    { pounds = 0; shillings = 0; pence = 0; farthings = total }

(*
  Type: bpd_imperial -> bpd_decimal
  Reason: Takes a bpd_imperial record and converts it to bpd_decimal.
    Uses the ratio 5/48 decimal pence per farthing (inverse of dec2imp).
    Integer arithmetic with `*`, `/`, and `mod` on ints.
*)
let imp2dec (v : bpd_imperial) : bpd_decimal =
  let total =
    v.farthings
    + v.pence     * 4
    + v.shillings * 48
    + v.pounds    * 960
  in
  let dp = total * 5 / 48 in
  { pounds = dp / 100; pence = dp mod 100 }

(*
  Type: bpd_imperial -> bpd_imperial -> bpd_imperial
  Reason: Infix alias for sum_imperial. Both operands and return value
    are bpd_imperial records.
*)
let ( +$ ) = sum_imperial

(*
  Type: bpd_imperial -> bpd_imperial -> bpd_imperial
  Reason: Infix alias for diff_imperial. Both operands and return value
    are bpd_imperial records.
*)
let ( -$ ) = diff_imperial


(* ===== Assignment 3.3 ===== *)

(*
  Type: 'a list -> int -> 'a
*)
let rec nth list i = match i with
  | 0 -> hd list
  | _ -> nth (tl list) (i - 1)

(*
  Type: int -> int list
*)
let iota n =
  let rec range a b =
    if a = b then [] else a::range (a + 1) b
  in
  range 0 n
  (*
    Alternatively, the body of this function could be written as:
    if n = 0 then [] else iota (n - 1) @ [n - 1]
    But this would be slower as we need to concatenate potentially long lists.
  *)

(*
  Type: 'a -> 'a list -> int
  Reason: `x` can be of any type that supports structural equality `=`.
    `l` is a list of that same type. The function returns an int count.
    Pattern matching on the list with `::` deconstructs head and tail.
*)
let rec number x l =
  if l = [] then 0
  else (if hd l = x then 1 else 0) + number x (tl l)

(*
  Type: 'a list * 'b list -> ('a * 'b) list
  Reason: The two input lists can have independent element types 'a and
    'b, since they are only ever paired together and never compared.
    Pattern matching on the pair of lists with `::` deconstructs both
    heads and tails simultaneously. Returns a list of pairs ('a * 'b).
*)
let rec pair (l, m) =
  if l = [] || m = [] then []
  else (hd l, hd m) :: pair (tl l, tl m)
