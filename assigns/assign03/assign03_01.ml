(* Concatenation Lists

   A `concatlist` is a list whose constructors are based on
   concatenation instead of cons.  It has a constructor for the empty
   list (Nil), a single element list (Single) and the concatentation
   of two lists (Concat).

   Implement a function `sort` which given

     l : a `concatlist`

   returns a regular list with the same element as `l` but in sorted
   order.  You should do this WITHOUT trying to first converting `l`
   into a regular list.  In particular, you CANNOT use the function
   `List.sort`.

   Example:
   let l = Concat (Concat (Single 3, Single 2), Concat (Single 1, Single 10))
   let _ = assert (sort l = [1;2;3;10])

*)

type 'a concatlist
  = Nil
  | Single of 'a
  | Concat of 'a concatlist * 'a concatlist

let sort (l : 'a concatlist) : 'a list =
  let rec merge_sort (l : 'a concatlist) : 'a list =
    match l with
    | Nil -> []
    | Single x -> [x]
    | Concat (l1, l2) ->
      let l1' = merge_sort l1 in
      let l2' = merge_sort l2 in
      merge l1' l2'
  and merge (l1 : 'a list) (l2 : 'a list) : 'a list =
    match l1, l2 with
    | [], _ -> l2
    | _, [] -> l1
    | h1 :: t1, h2 :: t2 ->
      if h1 <= h2 then h1 :: merge t1 l2 else h2 :: merge l1 t2
  in
  merge_sort l

  