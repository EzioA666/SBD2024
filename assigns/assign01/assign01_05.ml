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
  (* Determine the optimal line width *)
  let rec find_line_width width =
    if width = max_width || (len - width) mod width <= min_width then width
    else find_line_width (width + 1)
  in
  let line_width = find_line_width 1 in
  (* Function to extract a line *)
  let rec extract_line start_idx =
    if start_idx >= len then ""
    else 
      let end_idx = min (start_idx + line_width) len in
      let line = String.sub s start_idx (end_idx - start_idx) in
      if end_idx = len then line
      else line ^ "\n" ^ extract_line end_idx
  in
  extract_line 0


