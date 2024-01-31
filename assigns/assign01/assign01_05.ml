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
  let total_length = String.length s in
  (* Start with the simplest case where every line is max_width long *)
  let base_lines = total_length / max_width in
  let extra_chars = total_length mod max_width in
  let optimal_line_count =
    if extra_chars = 0 then base_lines
    else if extra_chars >= min_width then base_lines + 1
    else base_lines
  in
  let optimal_line_width =
    if optimal_line_count = base_lines + 1 then total_length / optimal_line_count
    else max_width
  in
  let rec build_lines current_index line_count =
    if line_count = 0 then ""
    else
      let line_length = if line_count = 1 then total_length - current_index else optimal_line_width in
      let line = String.sub s current_index line_length in
      line ^ (if line_count > 1 then "\n" else "") ^ (build_lines (current_index + line_length) (line_count - 1))
  in
  build_lines 0 optimal_line_count


