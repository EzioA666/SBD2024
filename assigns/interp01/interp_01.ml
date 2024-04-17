(* UTILITIES *)
let cons x xs = x :: xs
let explode s = List.of_seq (String.to_seq s)
let implode cs = String.of_seq (List.to_seq cs)
let is_digit c = '0' <= c && c <= '9'
let is_blank c = String.contains " \012\n\r\t" c
let is_upper_case c = 'A' <= c && c <= 'Z'

type 'a parser = char list -> ('a * char list) option

let satisfy f = function
  | c :: cs when f c -> Some (c, cs)
  | _ -> None

let char c = satisfy ((=) c)

let str s cs =
  let rec go = function
    | [], ds -> Some (s, ds)
    | c :: cs, d :: ds when c = d -> go (cs, ds)
    | _ -> None
  in go (explode s, cs)

let map f p cs =
  match p cs with
  | Some (x, cs) -> Some (f x, cs)
  | None -> None

let (>|=) p f = map f p
let (>|) p x = map (fun _ -> x) p

let seq p1 p2 cs =
  match p1 cs with
  | Some (x, cs) -> (
      match p2 cs with
      | Some (y, cs) -> Some ((x, y), cs)
      | None -> None
    )
  | None -> None

let (<<) p1 p2 = map fst (seq p1 p2)
let (>>) p1 p2 = map snd (seq p1 p2)

let map2 f p1 p2 =
  seq p1 p2 >|= fun (x, y) -> f x y

let optional p cs =
  match p cs with
  | Some (x, cs) -> Some (Some x, cs)
  | None -> Some (None, cs)

let rec many p cs =
  match p cs with
  | Some (x, cs) -> (
      match (many p cs) with
      | Some (xs, cs) -> Some (x :: xs, cs)
      | None -> Some ([x], cs)
    )
  | None -> Some ([], cs)

let many1 p = map2 cons p (many p)

let alt p1 p2 cs =
  match p1 cs with
  | Some x -> Some x
  | None ->
    match p2 cs with
    | Some x -> Some x
    | None -> None

let (<|>) = alt

let pure x cs = Some (x, cs)
let fail _ = None

let bind p f cs =
  match p cs with
  | Some (x, cs) -> f x cs
  | None -> None

let (>>=) = bind

let choice ps =
  List.fold_left (<|>) fail ps

let ws = many (satisfy is_blank)
let keyword w = str w << ws

let rec_parser p =
  pure () >>= p

let parse p s =
  match p (explode s) with
  | Some (x, []) -> Some x
  | _ -> None

(* END OF UTILITIES *)

(* ============================================================ *)

(* BEGINNING OF PROJECT CODE *)

type ident = string
type command
  = Drop                   (* drop *)
  | Swap                   (* swap *)
  | Dup                    (* dup *)
  | Trace                  (* . *)
  | Add                    (* + *)
  | Sub                    (* - *)
  | Mul                    (* * *)
  | Div                    (* / *)
  | Lt                     (* < *)
  | Eq                     (* = *)
  | Bind of ident          (* |> ID *)
  | Call of ident          (* # ID *)
  | If of program          (* ? prog ; *)
  | Def of ident * program (* def prog ; *)
  | Ident of ident         (* ID *)
  | Num of int             (* num *)
and program = command list

let parse_ident =  (* TODO *)
(*many1 (satisfy is_upper_case) >|= implode << ws*)
ws >> many1 (satisfy is_upper_case) >|= implode << ws

let debug_print result label =
  match result with
  | Some _ -> Printf.printf "Successfully parsed %s\n" label
  | None -> Printf.printf "Failed to parse %s\n" label

let parse_num =
  many1 (satisfy is_digit) >>= fun digits ->
  let number = int_of_string (implode digits) in
  debug_print (Some number) "number";
  pure (Num number)

let parse_add = 
    keyword "+" >| Add >>= fun result -> 
    Printf.printf "Parsed '+' for Add operation\n"; 
    pure result

let parse_number =
      ws >> many1 (satisfy is_digit) >>= fun digits ->
      pure (Num (int_of_string (implode digits))) << ws
          
(* You are not required to used this but it may be useful in
   understanding how to use `rec_parser` *)
(* Parsing numbers - ensuring whitespace is consumed after a number *)
(*let parse_num =
  ws >> many1 (satisfy is_digit) >>= fun digits ->
  pure (Num (int_of_string (implode digits))) << ws
let parse_add = 
    keyword "+" >>= fun _ -> 
    pure Add*)

  
let rec parse_com () =
  let parse_def =
    map2
      (fun id p -> Def (id, p))
      (keyword "def" >> parse_ident << ws)
      (parse_prog_rec () << char ';')
  in parse_def <|> (
    choice [
      keyword "drop" >| Drop;
      keyword "swap" >| Swap;
      keyword "dup" >| Dup;
      keyword "." >| Trace;
      keyword "+" >| Add;
      keyword "-" >| Sub;
      keyword "*" >| Mul;
      keyword "/" >| Div;
      keyword "<" >| Lt;
      keyword "=" >| Eq;
      keyword "|>" >> parse_ident >>= (fun id -> pure (Bind id));
      keyword "#" >> parse_ident >>= (fun id -> pure (Call id));
      keyword "?" >> (parse_prog_rec () >>= fun prog -> keyword ";" >> pure (If prog));
      parse_ident >>= (fun id -> pure (Ident id));
      parse_num;
    ]
  )(* TODO *)
and parse_prog_rec () =
  many ((rec_parser parse_com) << ws)

  


(* Function to apply a parser to a string input *)

(*let parse_prog s =
    let chars = explode s in  (* Convert the input string to a char list *)
    match (ws >> parse_prog_rec () << ws) chars with
    | Some (commands, []) -> Some commands  (* Successfully parsed the entire input *)
    | _ -> None  (* Parsing failed or did not consume all input *)
  (* TODO *)*)

let parse_prog s =
    (*let chars = explode s in
    match (ws >> parse_prog_rec () << ws) chars with
    | Some (commands, []) -> debug_print (Some (implode chars)) "program"; Some commands
    | _ -> Printf.printf "Failed to parse full input: %s\n" s; None*)
    let chars = explode s in  (* Convert the string to a list of characters *)
    match (ws >> parse_prog_rec () << ws) chars with
    | Some (commands, []) -> 
        debug_print (Some (implode chars)) "program"; 
        Some commands
    | Some (_, remaining) -> 
        Printf.printf "Failed to parse at: '%s'\n" (implode remaining);
        None
    | None -> 
        Printf.printf "Failed to parse full input: %s\n" s; 
        None
  
  

(* A VERY SMALL TEST SET *)
(*
let test = parse_prog "drop"
let out = Some [Drop]
let _ = assert (test = out)

let test = parse_prog "     .       "
let out = Some [Trace]
let _ = assert (test = out)

let test = parse_prog "  |> TEST   "
let out = Some [Bind "TEST"]
let _ = assert (test = out)

let test = parse_prog "  23 00345 + |> OK "
let out = Some [Num 23; Num 345; Add; Bind "OK"]
let _ = assert (test = out)

let test = parse_prog "  def NEG 0 - ; 2 #NEG 2 =    \n\n   "
let out = Some [Def ("NEG", [Num 0; Sub]); Num 2; Call "NEG"; Num 2; Eq]
let _ = assert (test = out)

let test = parse_prog "
  def ABS
    dup 0 swap < ?
      0 -
    ;
  ;

  30 0 -
  #ABS
  |> X
"
let out = Some
    [ Def ("ABS", [Dup; Num 0; Swap; Lt; If [Num 0; Sub]])
    ; Num 30; Num 0; Sub
    ;  Call "ABS"
    ;  Bind "X"
    ]
let _ = assert (test = out)
*)

(* EVALUATION *)

(* EVALUATION *)

type stack = int list
type value =
  | Num of int
  | Prog of program
type env = (ident * value) list
type trace = string list

(* Updated environment handling functions *)
let update_env (env: env) (id: ident) (v: value) =
  (id, v) :: env

let fetch_env (env: env) (id: ident): value option =
  let rec fetch env =
    match env with
    | [] -> None
    | (ide, curr_val) :: rest ->
        if ide = id then Some curr_val
        else fetch rest
  in
  fetch env

let string_of_stack stack =
    List.fold_right (fun elem acc -> Printf.sprintf "%s %d" acc elem) stack ""

let eval_command (stack, env, trace, cmds) cmd =
  match cmd with
  | Drop ->
      (match stack with
      | _ :: s_tail -> (s_tail, env, trace, cmds)
      | [] -> ([], env, "panic: stack underflow" :: trace, cmds))
  | Swap ->
      (match stack with
      | x :: y :: s_tail -> (y :: x :: s_tail, env, trace, cmds)
      | _ -> ([], env, "panic: insufficient stack for swap" :: trace, cmds))
  | Dup ->
      (match stack with
      | x :: s_tail -> (x :: x :: s_tail, env, trace, cmds)
      | [] -> ([], env, "panic: stack underflow on dup" :: trace, cmds))
  | Trace ->
      (match stack with
      | x :: s_tail -> (s_tail, env, (string_of_int x) :: trace, cmds)
      | [] -> ([], env, "panic: stack underflow on trace" :: trace, cmds))
  | Add ->
      (match stack with
      | x :: y :: s_tail -> ((x + y) :: s_tail, env, trace, cmds)
      | _ -> ([], env, "panic: insufficient stack for add" :: trace, cmds))
  | Sub ->
      (match stack with
      | x :: y :: s_tail -> ((x - y) :: s_tail, env, trace, cmds)
      | _ -> ([], env, "panic: insufficient stack for sub" :: trace, cmds))
  | Mul ->
      (match stack with
      | x :: y :: s_tail -> ((x * y) :: s_tail, env, trace, cmds)
      | _ -> ([], env, "panic: insufficient stack for mul" :: trace, cmds))
  | Div ->
      (match stack with
      | x :: 0 :: _ -> ([], env, "panic: division by zero" :: trace, cmds)
      | x :: y :: s_tail -> ((x / y) :: s_tail, env, trace, cmds)
      | _ -> ([], env, "panic: insufficient stack for div" :: trace, cmds))
  | Lt ->
      (match stack with
      | x :: y :: s_tail -> ((if x < y then 1 else 0) :: s_tail, env, trace, cmds)
      | _ -> ([], env, "panic: insufficient stack for lt" :: trace, cmds))
  | Eq ->
      (match stack with
      | x :: y :: s_tail -> ((if x = y then 1 else 0) :: s_tail, env, trace, cmds)
      | _ -> ([], env, "panic: insufficient stack for eq" :: trace, cmds))
  | Bind id ->
      (match stack with
      | x :: s_tail -> (s_tail, (id, Num x) :: env, trace, cmds)
      | [] -> ([], env, "panic: stack underflow on bind" :: trace, cmds))
  (*| Call id ->
      (match fetch_env env id with
      | Some (Prog p) -> (stack, env, trace, p @ cmds)
      | _ -> ([], env, "panic: call to non-existent or non-program identifier" :: trace, cmds))*)
  | Call id ->
        Printf.printf "Attempting to call subroutine: %s\n" id;
        (match fetch_env env id with
        | Some (Prog p) ->
            Printf.printf "Subroutine found, executing...\n";
            (stack, env, trace, p @ cmds)  
        | _ ->
            Printf.printf "Subroutine not found or not a prsogram.\n";
            ([], env, "panic: call to non-existent or non-program identifier" :: trace, cmds))
    
  | If p ->
        (match stack with
        | 0 :: s_tail -> (s_tail, env, trace, cmds)
        | n :: s_tail -> (s_tail, env, trace, p @ cmds)  
        | [] -> ([], env, "panic: stack underflow on if" :: trace, cmds))    
  | Def (id, p) -> (stack, update_env env id (Prog p), trace, cmds)  
  | Ident id ->
      (match fetch_env env id with
      | Some (Num n) -> (n :: stack, env, trace, cmds)
      | _ -> ([], env, "panic: identifier not bound to number" :: trace, cmds))
  | Num n -> (n :: stack, env, trace, cmds)

(*let eval_prog (stack, env, trace, prog) =
  let rec eval_loop (s, e, t, p) =
    match p with
    | [] -> (s, e, t)  (* Return the final state without the empty program *)
    | cmd :: cmds -> 
        let (new_stack, new_env, new_trace, _) = eval_command (s, e, t, cmds) cmd in
        eval_loop (new_stack, new_env, new_trace, cmds)
  in
  let (_, _, final_trace) = eval_loop (stack, env, trace, prog) in
  final_trace  (* Return only the trace as per project requirements *)*)
let eval_prog (stack, env, trace, prog) =
    let rec eval_loop (s, e, t, p) =
      match p with
      | [] -> (s, e, t)  (* No more commands to process *)
      | cmd :: rest -> 
          let (new_stack, new_env, new_trace, new_cmds) = eval_command (s, e, t, rest) cmd in
          eval_loop (new_stack, new_env, new_trace, new_cmds)  (* Use new_cmds from eval_command *)
    in
    let (_, _, final_trace) = eval_loop (stack, env, trace, prog) in
    final_trace  (* Return only the trace as per project requirements *)
  

let interp input =
  match parse_prog input with
  | Some prog ->
      let trace = eval_prog ([], [], [], prog) in
      Some trace
  | None -> None
      

(* END OF PROJECT CODE *)

(* ============================================================ *)

(* UNCOMMENT TO RUN INTERPRETER *)

let print_trace t =
  let rec go t =
    match t with
    | [] -> ()
    | x :: t ->
      print_endline x;
      go t
  in go (List.rev t)



let main () =
  let input =
    let rec get_input s =
      try
        get_input (s ^ "\n" ^ read_line ())
      with End_of_file ->
        s
    in get_input ""
  in
  match interp input with
  | None -> print_endline "Parse Error"
  | Some t -> print_trace t

let _ = main ()
