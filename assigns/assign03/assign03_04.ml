(* Matrices

   In this problem you will be building a (very) small interface for
   matrices.  A matrix is represented as a record which keeps track of
   its numbers of rows and columns as well as the values represented
   as a list of rows (i.e., a list of lists).  You will be
   implementing some error handling by working with `result`s.

   ************

   Task 1: Implement a function `mkMatrix` which given

     rs : a list of lists

   returns a matrix represented by this list of lists if it is valid
   or an `error` otherwise.  The error conditions are:

   * If the lengths of the rows in `rs` are not all equal then
   `mkMatrix` should return `Error UnevenRows`.

   * If `rs` is empty then `mkMatrix` should return `Error ZeroRows`.

   * If `rs` contains only empty lists, then `mkMatrix` should reutrn
   `Error ZeroCols`.

   In other words, `mkMatrix` should only return a matrix if `rs`
   represents a nonempty rectangular grid of values.

   Example:
   let l = [[1;2;3];[4;5;6]]
   let rm = mkMatrix l
   let _ = match rm with
     | Ok m ->
       let _ = assert (m.num_rows = 2) in
       let _ = assert (m.num_cols = 3) in
       ()
     | _ -> assert false

   let r = [[1;2;3;4];[1;2;3]]
   let rm' = mkMatrix r
   let _ = match rm' with
     | Ok _ -> assert false
     | Error e -> assert (e = UnevenRows)

   ************

   Task 2: Implement the function `transpose` which, given

     m : a matrix

   returns the transpose of `m`.

   Example:
   let _ = match rm with
     | Ok m ->
       let tm = transpose m in
       let _ = assert (tm.num_rows = 3) in
       let _ = assert (tm.num_cols = 2) in
       let _ = assert (tm.rows = [[1;4];[2;5];[3;6]]) in
       ()
     | _ -> assert false

   ************

   Task 3: Implement the function `multiply` which, given

     m : a matrix
     n : a matrix

   returns the product of `m` and `n` if it is possible to multiply
   them. The error condition:

   * If the number of columns of `m` is not the same as the number of
   rows of `n`, then `multiply` should return `Error MulMismatch`

   Example:
   let a =
     { num_rows = 2 ;
       num_cols = 2 ;
       rows = [[1.;2.];[3.;4.]] ;
     }

   let b =
     { numn_rows = 1 ;
       num_cols = 2 ;
       rows = [[1.; 2.]] ;
     }

   let _ = assert (multiply a a = Ok {
     num_rows = 2 ;
     num_cols = 2 ;
     rows = [[7.;10.];[15.;22.]] ;
    })

   let _ = assert (multiply a b = Error MulMismatch)

   ************

   References:
   https://en.wikipedia.org/wiki/Matrix_multiplication
   https://en.wikipedia.org/wiki/Transpose
   https://www.cs.bu.edu/fac/crovella/cs132-book/landing-page.html

*)

type error
   = UnevenRows
   | ZeroRows
   | ZeroCols
   | MulMismatch

type 'a matrix = {
  num_rows : int ;
  num_cols : int ;
  rows : ('a list) list ;
}

let mkMatrix (rs : 'a list list) : ('a matrix, error) result =
  let rec check_rows lengths = match lengths with
  | [] -> true
  | [x] -> true
  | x :: y :: xs -> x = y && check_rows (y :: xs)
in
match rs with
| [] -> Error ZeroRows
| [[]] -> Error ZeroCols
| _ -> 
    let row_lengths = List.map List.length rs in
    if not (check_rows row_lengths) then Error UnevenRows
    else 
      let num_rows = List.length rs in
      let num_cols = List.hd row_lengths in
      if num_cols = 0 then Error ZeroCols
      else Ok {num_rows; num_cols; rows = rs}

let transpose (m : 'a matrix) : 'a matrix =
  let rec transpose_helper rows =
    match rows with
    | [] | [] :: _ -> []
    | rows -> 
      (List.map List.hd rows) :: transpose_helper (List.map List.tl rows)
  in
  {num_rows = m.num_cols; num_cols = m.num_rows; rows = transpose_helper m.rows}

let multiply (m : float matrix) (n : float matrix) : (float matrix, error) result =
  if m.num_cols <> n.num_rows then Error MulMismatch
  else 
    let rec dot_product a b = match (a, b) with
      | ([], []) -> 0.0
      | (x :: xs, y :: ys) -> x *. y +. dot_product xs ys
      | _ -> failwith "Mismatched lengths" (* This should not happen due to checks. *)
    in
    let rec multiply_row_by_matrix row matrix =
      match matrix with
      | [] -> []
      | cols :: rest -> dot_product row (List.map List.hd matrix) :: multiply_row_by_matrix row (List.map List.tl matrix)
    in
    let rows = List.map (fun row -> multiply_row_by_matrix row (transpose n).rows) m.rows in
    Ok {num_rows = m.num_rows; num_cols = n.num_cols; rows}
