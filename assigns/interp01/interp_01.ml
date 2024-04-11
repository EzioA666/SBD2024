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
many1 (satisfy is_upper_case) >|= implode << ws



(* You are not required to used this but it may be useful in
   understanding how to use `rec_parser` *)
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
      (keyword "|>" >> parse_ident >>= fun id -> pure (Bind id));
      (keyword "#" >> parse_ident >>= fun id -> pure (Call id));
      (keyword "?" >> parse_prog_rec () << keyword ";" >>= fun prog -> pure (If prog));
      (parse_ident >>= fun id -> pure (Ident id));
      (many1 (satisfy is_digit) >>= fun digits -> pure (Num (int_of_string (implode digits))) << ws)
    ]
  )(* TODO *)
and parse_prog_rec () =
  many ((rec_parser parse_com) << ws)


(* Function to apply a parser to a string input *)

  let parse_prog s =
    let chars = explode s in  (* Convert the input string to a char list *)
    match (ws >> parse_prog_rec () << ws) chars with
    | Some (commands, []) -> Some commands  (* Successfully parsed the entire input *)
    | _ -> None  (* Parsing failed or did not consume all input *)
  (* TODO *)
  

(* A VERY SMALL TEST SET *)

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


(* EVALUATION *)

type stack = int list
type value
  = Num of int
  | Prog of program
type env = (ident * value) list
type trace = string list

let update_env env id value = (id, value) :: env
let lookup_env env id = List.assoc_opt id env
(* A helper function to process a single command *)
let eval_command config command =
  let (stack, env, trace, prog) = config in
  match command with
  | Drop -> 
      (match stack with
      | _ :: s_tail -> (s_tail, env, trace, prog)
      | [] -> ([], env, "panic: stack underflow" :: trace, prog))
  (* Add cases for other commands here *)
  | _ -> config

let eval_prog (stack, env, trace, prog) =
  let rec eval_loop (s, e, t, p) =
    match p with
    | [] -> (s, e, t, p)
    | cmd :: cmds -> eval_loop (eval_command (s, e, t, cmds) cmd)
  in
  eval_loop (stack, env, trace, prog)

let interp input =
    match parse_prog input with
    | Some prog -> 
        let final_config = eval_prog ([], [], [], prog) in
        let (_, _, trace, _) = final_config in
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
    let inputs = [
      "10 20 swap";
      "5 dup";
      "10 20 add";
      "20 5 sub";
      "2 3 mul";
      "10 0 div";  (* This should handle or prevent division by zero *)
    ] in
    List.iter (fun input -> 
      Printf.printf "Testing input: %s\n" input;
      match interp input with
      | None -> print_endline "Parse Error"
      | Some t -> print_trace t
    ) inputs
  
let _ = main ()
  
  
  
(*
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
*)
