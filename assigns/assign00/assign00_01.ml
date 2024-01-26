(* Find square-root 

   Please implement a function `sqrt` of type `int -> int` such that
   when given an integer `n` greater than or equal to 0, it returns its
   square root. For inputs whose square roots are not whole numbers,
   return the smallest number `i` such that `n <= i * i`. 

   Examples with whole square roots:
   sqrt 4 = 2
   sqrt 9 = 3
   sqrt 100 = 10

   Examples without whole square roots:
   sqrt 2 = 2        (smallest number i such that 2 <= i * i is 2)
   sqrt 10 = 4       (smallest number i such that 10 <= i * i is 4)
   sqrt 99 = 10      (smallest number i such that 99 <= i * i is 10)

*)


let sqrt (n : int) : int =
      let rec aux low high =
         if low > high then high
         else
           let mid = low + (high - low) / 2 in
           let mid_sq = mid * mid in
           if mid_sq = n then mid
           else if mid_sq < n then aux (mid + 1) high
           else aux low (mid - 1)
       in
       if n = 0 then 0 else aux 1 n