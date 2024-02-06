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
  let move point dir =
    match dir with
    | N -> { x = point.x; y = point.y + 1 }
    | S -> { x = point.x; y = point.y - 1 }
    | E -> { x = point.x + 1; y = point.y }
    | W -> { x = point.x - 1; y = point.y }
  in
  let add_dir_to_path path dir =
    match path with
    | (d, n) :: rest when d = dir -> (d, n + 1) :: rest
    | _ -> (dir, 1) :: path
  in
  if len = 0 then
    if stp = endp then [[]] else []
  else
    let directions = [N; S; E; W] in
    List.fold_left (fun acc dir ->
      let next_point = move stp dir in
      let sub_paths = all_paths (len - 1) next_point endp in
      List.fold_left (fun acc_sub path ->
        (add_dir_to_path path dir) :: acc_sub
      ) acc sub_paths
    ) [] directions
  |> List.filter (fun path ->
       let rec final_position point path =
         match path with
         | [] -> point
         | (dir, steps) :: rest ->
           final_position (List.init steps (fun _ -> move point dir) |> List.fold_left (fun acc _ -> move acc dir) point) rest
       in
       final_position stp (List.rev path) = endp
     )