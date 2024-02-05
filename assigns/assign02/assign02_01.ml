(* Grouping Integers and Strings

   Implement a function `convert` which given

     l : a list of `int_or_string`s

   returns a list of `int_list_or_string_list`s such that adjacent
   `int`s and `string`s are grouped together.

   Example:
   let test_in = [Int 2; Int 3; String "a"; String "b"; Int 4; String "c"]
   let test_out = [IntList [2;3]; StringList ["a";"b"]; IntList [4]; StringList ["c"]]
   let _ = assert (convert test_in = test_out)

*)

type int_list_or_string_list
  = IntList of int list
  | StringList of string list

type int_or_string
  = Int of int
  | String of string

let convert (l : int_or_string list) : int_list_or_string_list list =
    let rec aux acc current_ints current_strings = function
      | [] -> (
          match (current_ints, current_strings) with
          | ([], []) -> List.rev acc
          | (_, []) -> List.rev (IntList (List.rev current_ints) :: acc)
          | ([], _) -> List.rev (StringList (List.rev current_strings) :: acc)
        )
      | (Int i :: t) ->
          if current_strings <> [] then
            aux (StringList (List.rev current_strings) :: acc) [i] [] t
          else
            aux acc (i :: current_ints) [] t
      | (String s :: t) ->
          if current_ints <> [] then
            aux (IntList (List.rev current_ints) :: acc) [] [s] t
          else
            aux acc [] (s :: current_strings) t
    in
    aux [] [] [] l
