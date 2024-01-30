(* Block text

   Please implement the function `block_text` of type `string ->
   string` which, given

   - a string `s` consisting only of capital English characters A-Z
   - a nonnegative integer `min_width`
   - a positive integer `max_width`

   returns a string with the same characters as `s` but separated into
   lines with the following properties:

   - each line has length at most `max_width`
   - each line has the same length except possibly the last line
   - the last line has length at least `min_width`

   If the above three properties are not possible to satisfy, then
   every line except for the last line of the returned string should
   have length `max_width` (in other words, ignore the last
   condition).

   If there are multiple ways to satisfy the above three properties,
   the choice with the longest lines (besides the last line) should be
   used.

   Hint: Take a look at the following functions in the OCaml
   documentation:

   - `String.sub`
   - `String.length`

   Examples:
   let _ = assert (block_text "ABCDEFGHIJ" 0 3 = "ABC\nDEF\nGHI\nJ")
   let _ = assert (block_text "ABCDEFGHIJ" 2 3 = "AB\nCD\nEF\nGH\nIJ")
   let _ = assert (block_text "ABCDEFGHIJ" 0 4 = "ABCD\nEFGH\nIJ")
   let _ = assert (block_text "ABDFEFGHIJ" 3 4 = "ABCD\nEFGH\nIJ")

 *)

 let block_text (s : string) (min_width : int) (max_width : int) : string =
  let len = String.length s in
  (* Determine the appropriate line length *)
  let line_length = 
    let rec find_length current_length =
      if current_length > max_width || len - current_length <= current_length then current_length
      else find_length (current_length + 1)
    in
    find_length (max min_width 1)
  in
  (* Recursive function to split the string *)
  let rec split_string index result =
    if index >= len then result
    else
      let next_index = min (index + line_length) len in
      let line = String.sub s index (next_index - index) in
      let new_result = if result = "" then line else result ^ "\n" ^ line in
      split_string next_index new_result
  in
  split_string 0 ""

