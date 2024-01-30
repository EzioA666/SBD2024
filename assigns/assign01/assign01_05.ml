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

  let find_optimal_length () =
    let rec aux possible_line_count =
      if possible_line_count = 0 then max_width (* Fallback to max_width *)
      else
        let line_length = len / possible_line_count in
        if line_length <= max_width && (len - line_length * (possible_line_count - 1)) >= min_width then
          line_length
        else
          aux (possible_line_count - 1)
    in
    if len <= max_width then len else aux (len / min_width)
  in
  let line_length = find_optimal_length () in

  let rec build_lines i result =
    if i >= len then result
    else
      let end_index = min (i + line_length) len in
      let line = String.sub s i (end_index - i) in
      build_lines end_index (result ^ (if i = 0 then "" else "\n") ^ line)
  in
  build_lines 0 ""

