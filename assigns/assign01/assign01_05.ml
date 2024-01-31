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
   let _ = assert (block_text "ABCDEFGHIJ" 3 4 = "ABCD\nEFGH\nIJ")

 *)

 let block_text (s : string) (min_width : int) (max_width : int) : string =
    let len = String.length s in
    (* A helper function to determine the optimal line width *)
    let rec find_optimal_line_width n =
      if n > max_width || (len mod n) < min_width && (len mod n) != 0 then
        find_optimal_line_width (n - 1)
      else
        n
    in
    let optimal_width =
      if len <= max_width then len
      else find_optimal_line_width (len / (len / max_width))
    in
    (* Build the string with the calculated line width *)
    let rec build_string acc index =
      if index >= len then
        acc
      else
        let next_index = min (index + optimal_width) len in
        let line = String.sub s index (next_index - index) in
        build_string (acc ^ (if acc = "" then "" else "\n") ^ line) next_index
    in
    build_string "" 0
  


