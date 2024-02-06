(* Icy Hot

   Implement the function `reduce` which given

     l : a list of `temp`s

   returns a new list of `temp`s gotten by the following reduction rule:

   If `Hot i` and `Icy i` are adjacent (in particular, they must be
   carrying the same value) in any order, then they cancel out and are
   removed from the list.

   This rule should be carried out until it not possible to reduce the
   list any further.

   Examples:
   let _ = assert (reduce [Hot 0;Icy 0] = [])
   let _ = assert (reduce [Hot 0;Icy 1;Hot 1;Icy 0] = [])
   let _ = assert (reduce [Hot 0;Hot 1;Icy 2;Hot 2;Hot 0;Icy 1] = [Hot 0;Hot 1;Hot 0;Icy 1])

*)

type temp
  = Hot of int
  | Icy of int

let reduce (l : temp list) : temp list =
  let rec aux acc = function
    | [] -> List.rev acc  (* Reverse acc to restore the original order *)
    | [x] -> List.rev (x :: acc)  (* Handle the last element *)
    | Hot i :: Icy j :: t when i = j -> aux acc t  (* If Hot i is adjacent to Icy i, skip both *)
    | Icy i :: Hot j :: t when i = j -> aux acc t  (* If Icy i is adjacent to Hot i, skip both *)
    | h :: t -> aux (h :: acc) t  (* Otherwise, add the current element to acc and continue *)
  in
  let rec reduce_until_stable old_list =
    let new_list = aux [] old_list in
    if new_list = old_list then new_list  (* If no changes, return the list *)
    else reduce_until_stable new_list  (* Otherwise, try reducing again *)
  in
  reduce_until_stable l
