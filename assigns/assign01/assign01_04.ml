(* Taxicab numbers

   A taxicab number is a positive integer which can be expressed as
   the sum of two positive cubes in more than one way. (This is
   slightly different than the usual definition.)

   Please implement the function `taxicab` of type `int -> int` which,
   given a positive integer `n`, returns the the number of ways that
   `n` can be expressed as the sum of two positive cubes.

   Examples:
   let _ = assert (taxicab 2 = 1)
   let _ = assert (taxicab 5 = 0)
   let _ = assert (taxicab 1729 = 2)   (* 1729 = 1^3 + 12^3 = 3^3 + 9^3 *)
   let _ = assert (taxicab 4104 = 2)   (* 4104 = 2^3 + 16^3 = 9^3 + 15^3 *)

 *)

let taxicab (n : int) : int =
    let max_val = int_of_float (float_of_int n ** (1. /. 3.)) in
    let rec find_pairs x y count =
      if x > max_val then count
      else if y > max_val then find_pairs (x + 1) (x + 1) count
      else 
        let cube_sum = x * x * x + y * y * y in
        if cube_sum > n then find_pairs x (y + 1) count
        else if cube_sum < n then find_pairs (x + 1) y count
        else find_pairs (x + 1) (y + 1) (count + 1)
    in
    find_pairs 1 1 0
  
