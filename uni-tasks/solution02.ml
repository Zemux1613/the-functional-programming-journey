(*
  Type: int -> int -> int
  Reason: `exp` is matched against integers, `num` is used with the `*` operator.
*)
let rec power num exp = match exp with
    | 0 -> 1
    | 1 -> num (* this branch is optional, but saves us an invocation in a trivial case. *)
    | _ -> num * power num (exp - 1)

(*
  Type: string -> int -> char -> string
  Reason: `str` is used in concatenation, `n` is matched against an integer, `sep` is passed to a
      function expecting a char.
*)
let rec powers str n sep = match n with
    | 0 -> ""
    | 1 -> str (* this branch is needed to avoid a trailing separator *)
    | _ -> str ^ (String.make 1 sep) ^ powers str (n - 1) sep

(*
  Type: int -> int
  Reason: `n` is used with `mod` and `/` operators, which require int operands.
          The function returns the accumulated digit `sum`, which is of type int.
*)
let rec digitalRoot (n : int) : int =
  let sum = ref 0 in
  let m = ref n in
  while !m > 0 do
    sum := !sum + !m mod 10;
    m := !m / 10
  done;
  if !sum < 10 then !sum else digitalRoot !sum ;;
  
(*
  Type: string -> bool
  Reason: `str` is passed to `String.length`, return value may be coming from a boolean literal.
*)
let rec isPalindrome str = match String.length str with
    | 0 (* this is debatable *)
    | 1 -> true
    | len -> str.[0] = str.[len - 1] && isPalindrome (String.sub str 1 (len - 2))
