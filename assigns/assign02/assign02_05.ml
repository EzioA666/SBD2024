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
  (* Helper function to move a point in a given direction *)
  let move (p : point) (d : dir) (dist : int) : point =
    match d with
    | N -> {p with y = p.y + dist}
    | S -> {p with y = p.y - dist}
    | E -> {p with x = p.x + dist}
    | W -> {p with x = p.x - dist}
  in

  (* Helper function to check if a path ends at the end point *)
  let rec path_ends_at (p : point) (path : (dir * int) list) : bool =
    match path with
    | [] -> p = endp
    | (d, dist) :: rest -> path_ends_at (move p d dist) rest
  in

  (* Generate all possible sequences of moves *)
  let rec generate_paths (len : int) (current_path : (dir * int) list) : (dir * int) list list =
    if len = 0 then
      if path_ends_at stp current_path then [List.rev current_path] else []
    else
      let dirs = [N; S; E; W] in
      List.fold_left (fun acc d ->
        if len = 1 || (current_path <> [] && fst (List.hd current_path) = d) then acc
        else
          List.append acc (generate_paths (len - 1) ((d, 1) :: current_path))
      ) [] dirs
  in

  (* Function to merge consecutive steps in the same direction *)
  let rec merge_consecutive (path : (dir * int) list) : (dir * int) list =
    match path with
    | (d1, n1) :: (d2, n2) :: rest when d1 = d2 -> merge_consecutive ((d1, n1 + n2) :: rest)
    | head :: rest -> head :: merge_consecutive rest
    | [] -> []
  in

  let raw_paths = generate_paths len [] in
  List.map merge_consecutive raw_paths
