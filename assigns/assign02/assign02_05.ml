(* Listing Paths

   Implement a function `all_paths` which given

     len : a nonnegative integer
     stp : a point (see below)
     endp : a point

   returns a list of `(dir * int)` lists, where each list contains
   sequence of directions to get from `stp` to `endp` in exactly `len`
   steps.

   Notes:
   A sequence of directions is a list of `dir * int` pairs with the
   property that no adjacent elements have the same `dir`.  For example,

     [(N, 1); (S, 1); (N, 3)]

   is a valid sequence of directions but

     [(N, 1); (N, 3); (S, 1)]

   is not. Instead, this should be

     [(N, 4), (S, 1)]

   Examples:
   let origin = {x=0;y=0}
   let _ = assert (all_paths 0 origin origin = [[]])
   let _ = assert (all_paths 1 origin origin = [])
   let _ = assert (all_paths 2 origin origin =
       [[(N,1);(S,1)] ;
        [(S,1);(N,1)] ;
        [(E,1);(W,1)] ;
        [(W,1);(E,1)] ])
   let _ = assert (all_paths 3 origin origin = [])
   let _ = assert (List.mem [(N,2);(S,2)] (all_paths 4 origin origin))
   let _ = assert (List.mem [(N,1);(E,1);(W,1);(S,1)] (all_paths 4 origin origin))

*)

type dir = N | S | E | W

type point = {
  x : int ;
  y : int ;
}

let rec all_paths (len : int) (stp : point) (endp : point) : (dir * int) list list =
  let rec explore len (x, y) (target_x, target_y) path =
    if len = 0 then
      if x = target_x && y = target_y then [List.rev path] else []
    else if abs (target_x - x) + abs (target_y - y) > len then
      []
    else
      let moves = 
        [N, (x, y + 1); S, (x, y - 1); E, (x + 1, y); W, (x - 1, y)] in
      List.fold_left (fun acc (dir, (nx, ny)) ->
        if path <> [] && fst (List.hd path) = dir then acc
        else explore (len - 1) (nx, ny) (target_x, target_y) ((dir, 1)::path)
      ) [] moves
  in
  explore len (stp.x, stp.y) (endp.x, endp.y) []
