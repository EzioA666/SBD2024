(* Perfect numbers

   A positive integer `n` is perfect if it is equal to the sum of its
   proper divisors.

   Please implement the function `is_perfect` of type `int -> bool`
   which, given an positive integer `n`, returns `true` if `n` is
   perfect and `false` otherwise.

   Examples:
   let _ = assert (is_perfect 6)        (* 1 + 2 + 3 = 6 *)
   let _ = assert (is_perfect 28)       (* 1 + 2 + 4 + 7 + 14 = 28 *)
   let _ = assert (not (is_perfect 24)) (* 1 + 2 + 3 + 4 + 6 + 8 + 12 <> 24 *)

 *)

let is_perfect (n : int) : bool =
    let rec helper i sum =
        if i > n / 2 then sum = n  (* we just need to compare up to n/2*)
        else if n mod i = 0 then helper (i + 1) (sum + i)(* If i is a divisor, add it to sum and continue *)
        else helper (i + 1) sum  
    in
    if n <= 1 then false(*1 or less is not perfect *)
    else helper 1 0

