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
  (* Function to calculate the optimal line length *)
  let rec find_optimal_length l =
    if l >= max_width || (len mod l) >= min_width || len = l then l
    else find_optimal_length (l + 1)
  in
  let line_length = find_optimal_length 1 in
  (* Recursive function to split the string and build the result *)
  let rec build_lines remaining start result =
    if String.length remaining <= line_length then
      result ^ (if result = "" then "" else "\n") ^ remaining
    else
      let line = String.sub remaining 0 line_length in
      let new_remaining = String.sub remaining line_length (String.length remaining - line_length) in
      build_lines new_remaining (start + line_length) (result ^ (if start = 0 then "" else "\n") ^ line)
  in
  build_lines s 0 ""

