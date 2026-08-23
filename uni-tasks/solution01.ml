(*
  Type: int -> int
  Reason: `mod` expects two integers, and returns an integer.
*)
let firstDigit n = n mod 10

(*
  Type: int -> float -> float
  Reason: `digits` is compared to the integer literal `0` a few times (well, after being passed to
    mul_n and div_n as a parameter), and `n` is used with floating-pointer operators like `*.`.
*)
let round digits n =
    let rec mul_n digits n = if digits = 0 then n else mul_n (digits - 1) (n *. 10.) in
    let multiplied = mul_n digits n in
    let trunc = Float.trunc multiplied in
    let rec div_n digits n = if digits = 0 then n else div_n (digits - 1) (n /. 10.) in
    div_n digits trunc

(*
  Type: float -> float
  Reason: Round (of type int -> float -> float) is curried with an integer literal.
*)
let round2 = round 2

(*
  Type: int -> bool
  Reason: `year` is operated on with relational operators with integers on the right-hand side,
      returned value is coming from a logical conjunction.
*)
let isLeapYear year =
    year > 1582 && ((year mod 4 = 0 && year mod 100 != 0) || year mod 400 = 0)

(*
    Type: (int, int, int) -> bool
    Reason: The argument must be a tuple due to the syntax used, all elements of the tuples must be
      integers as they're being compared with integers or passed to a function expecting an integer.
*)
let isDate (d, m, y) = d >= 1 && m >= 1 && m <= 12 &&
    let days_in_month = match m with
        |  4
        |  6
        |  9
        | 11 -> 30

        |  1
        |  3
        |  7
        |  5
        |  8
        | 10
        | 12 -> 31

        |  2 -> if isLeapYear y then 29 else 28
        | _ -> assert false (* unreachable *)
    in
    d <= days_in_month

(*
  The suffix that should be appended to a number printed as part of a date.
  Type: int -> string

  Reason: `n` is being compared to integers, returned value is coming from a string literal.
*)
let englishSuffix n =
    if n >= 10 && n <= 20 then "th" else
        match firstDigit n with
        | 0
        | 4
        | 5
        | 6
        | 7
        | 8
        | 9 -> "th"

        | 1 -> "st"
        | 2 -> "nd"
        | 3 -> "rd"
        | _ -> assert false (* unreachable *)


(*
  The name of the n'th month of the year.  Caller asserts that 1 <= n <= 12.
  Type: int -> string
  Reason: `n` is matched against integer literals, returned value is coming from a string literal.
*)
let monthName n = match n with
    |  1 -> "January"
    |  2 -> "February"
    |  3 -> "March"
    |  4 -> "April"
    |  5 -> "May"
    |  6 -> "June"
    |  7 -> "July"
    |  8 -> "August"
    |  9 -> "September"
    | 10 -> "October"
    | 11 -> "November"
    | 12 -> "December"
    | _ -> assert false (* unreachable *)

(*
  Type: (int, int, int) -> string
  Reason: Argument must be a tuple due to syntax, all values are passed to functions expecting
    integers.
*)
let date2str ((d, m, y) as date) =
    let () = assert (isDate date) in
    (monthName m) ^ " " ^ (string_of_int d) ^ (englishSuffix d) ^ ", " ^ (string_of_int y)
