(* UTILITIES *)
let cons x xs = x :: xs
let explode s = List.of_seq (String.to_seq s)
let implode cs = String.of_seq (List.to_seq cs)
let is_digit c = '0' <= c && c <= '9'
let is_blank c = String.contains " \012\n\r\t" c
let is_upper_case c = 'A' <= c && c <= 'Z'
let is_lower_case c = 'a' <= c && c <= 'z'

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
let ( let* ) = bind

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

(* REQUIRED TYPES *)

type ident = string

type const
  = Num of int
  | Bool of bool

type command
  = Push of const | Trace
  | Add | Mul | Div 
  | And | Or | Not | Lt | Eq
  | If of program * program
  | While of program * program
  | Bind of ident | Fetch of ident
  | Fun of program | Call | Return
  | Debug of string

and program = command list

and bindings = (ident * value) list

and value
  = Const of const
  | Clos of
      { def_id : int
      ; captured : bindings
      ; prog : program
      }

type record =
  { id : int
  ; local : bindings
  ; called_def_id : int
  ; return_prog : program
  }

type stack = value list
type trace = string list
type env
  = Global of bindings
  | Local of record * env

(* get the id of the topmost record *)
let local_id = function
  | Global _ -> 0
  | Local (r, _) -> r.id

(* convert a value to a string *)
let to_string v =
  match v with
  | Const (Bool true) -> "True"
  | Const (Bool false) -> "False"
  | Const (Num n) -> string_of_int n
  | Clos _ -> "<Closure>"

(* PARSING *)

let parse_ident =
  map2
    (fun c cs -> implode (c :: cs))
    (satisfy is_lower_case)
    (many (satisfy (fun c -> is_lower_case c || is_digit c || c = '_')))

let parse_int =
  let mk_int sign cs =
    let abs = int_of_string (implode cs) in
    if Option.is_none sign
    then abs
    else -abs
  in
  map2
    mk_int
    (optional (char '-'))
    (many1 (satisfy is_digit))

let parse_bool =
  (str "True" >| true) <|> (str "False" >| false)

let parse_Bconst = parse_bool >|= fun x -> Bool x
let parse_Nconst = parse_int >|= fun x -> Num x
let parse_const = parse_Bconst <|> parse_Nconst

let parse_comment =
  char '(' >> many (satisfy ((<>) ')')) >> char ')' >| ()

let parse_debug =
  char '"' >> many (satisfy ((<>) '"')) << char '"' >|= implode

let ws = many (ws >> parse_comment) >> ws
let keyword w = str w << ws

let rec parse_com () =
  let parse_fun =
    let* _ = keyword ":" in
    let* body = parse_prog_rec () in
    let* _ = keyword ";" in
    pure (Fun body)
  in
  let parse_arith =
    choice
    [ char '+' >| Add
    ; char '*' >| Mul
    ; char '/' >| Div
    ; char '=' >| Eq
    ; char '<' >| Lt
    ]
  in
  let parse_logic =
    choice
    [ keyword "&&" >| And
    ; keyword "||" >| Or
    ; char '~' >| Not
    ]
  in
  let parse_bind =
    let* _ = keyword "|>" in
    let* id = parse_ident in
    pure (Bind id) 
  in
  let parse_fetch =
    parse_ident >|= fun id -> Fetch id
  in
  let parse_trace =
    keyword "." >| Trace
  in
  let parse_call =
    keyword "#" >| Call
  in
  let parse_return =
    keyword "Return" >| Return
  in
  let parse_if =
    let* _ = keyword "?" in
    let* ifc = parse_prog_rec () in
    let* _ = keyword ";" in
    let* elsec = parse_prog_rec () in
    let* _ = char ';' in
    pure (If (ifc, elsec))
  in
  let parse_while =
    let* _ = keyword "While" in
    let* check = parse_prog_rec () in
    let* _ = keyword ";" in
    let* body = parse_prog_rec () in
    let* _ = char ';' in
    pure (While (check, body))
  in
  choice
    (* TODO: Add more alternatives *)
    [ parse_fun
    ; parse_const >|= (fun s -> Push s)
    ; parse_arith 
    ; parse_logic 
    ; parse_bind 
    ; parse_fetch 
    ; parse_trace 
    ; parse_call 
    ; parse_return 
    ; parse_while
    ; parse_if
    ; parse_ident >|= (fun s -> Fetch s)
    ; parse_debug >|= (fun s -> Debug s)
    ]
and parse_prog_rec () =
  many (rec_parser parse_com << ws)

let parse_prog = parse (ws >> parse_prog_rec ())

(* FETCHING AND UPDATING *)

(* fetch the value of `x` in the environment `e` *)
let rec fetch_env e x =  (* TODO *)
match e with
| Global bindings -> List.assoc_opt x bindings 
| Local (record, enclosing_env) -> 
  (match List.assoc_opt x record.local with
  | Some _ as result -> result 
  | None -> fetch_env enclosing_env x) 

let rec update_env e x v = (* TODO *)
match e with
| Global bindings -> Global ((x, v) :: bindings) 
| Local (record, enclosing_env) ->
  if List.exists (fun (var, _) -> var = x) record.local then
    let updated_locals = List.map (fun (var, value) -> if var = x then (x, v) else (var, value)) record.local in
    Local ({record with local = updated_locals}, enclosing_env)
  else
    let updated_enclosing = update_env enclosing_env x v in
    Local (record, updated_enclosing)

(* EVALUTION *)


(* make the panic configuration given a configuration *)
(*let panic (_, _, t, _) msg = [], Global [], ("panic: " ^ msg) :: t, []*)

(* Panic function to handle errors *)
let panic (_, _, t, _) msg = [], Global [], ("panic: " ^ msg) :: t, []

(* Utility function to get the current scope identifier *)

let local_id = function
  | Global _ -> 0  (* Global scope has ID 0 *)
  | Local (r, _) -> r.id  (* Local scope has specific ID *)

let eval_step (c : stack * env * trace * program) =
  match c with
  (*while and if else*)
  | (stack, env, trace, While (cond_p, body_p) :: rest_p) ->
    (* Push a modified If command to handle the loop *)
    let continue_loop = body_p @ [While (cond_p, body_p)]  (* Append While to recheck after body *)
    and exit_loop = rest_p  (* Simply continue with the rest if condition is false *)
    in
    (stack, env, trace, cond_p @ [If (continue_loop, exit_loop)] @ rest_p)
  | (stack, env, trace, If (then_p, else_p) :: rest_p) ->
    
    begin match stack with
    | Const (Bool true) :: s -> (s, env, trace, then_p @ rest_p)
    | Const (Bool false) :: s -> (s, env, trace, else_p @ rest_p)
    | _ -> panic c "Invalid If condition: expected Boolean on stack"
    end
  (* Push *)
  | (s, e, t, Push (Num n) :: p) -> (Const (Num n) :: s, e, t, p)
  | (s, e, t, Push (Bool b) :: p) -> (Const (Bool b) :: s, e, t, p)
  (* Trace *)
  | v :: s, e, t, Trace :: p -> s, e, to_string v :: t, p
  | [], _, _, Trace :: _ -> panic c "stack underflow (. on empty)"
  (* Add *)
  | Const (Num m) :: Const (Num n) :: s, e, t, Add :: p -> Const (Num (m + n)) :: s, e, t, p
  | _ :: _ :: _, _, _, Add :: _ -> panic c "type error (+ on non-integers)"
  | _ :: [], _, _, Add :: _ -> panic c "stack underflow (+ on single)"
  | [], _, _, Add :: _ -> panic c "stack underflow (+ on empty)"
  (* TODO *)
  (*Mul Div shit*)
  | Const (Num m) :: Const (Num n) :: s, e, t, Mul :: p -> Const (Num (m * n)) :: s, e, t, p
  | Const (Num m) :: Const (Num n) :: s, e, t, Div :: p when n != 0 -> Const (Num (m / n)) :: s, e, t, p
  | Const (Num _) :: Const (Num 0) :: _, _, _, Div :: _ -> panic c "division by zero"
  (*Logical Operations*)
  | Const (Bool b1) :: Const (Bool b2) :: s, e, t, And :: p -> Const (Bool (b1 && b2)) :: s, e, t, p
  | Const (Bool b1) :: Const (Bool b2) :: s, e, t, Or :: p -> Const (Bool (b1 || b2)) :: s, e, t, p
  | Const (Bool b) :: s, e, t, Not :: p -> Const (Bool (not b)) :: s, e, t, p
  (*Bind*)
  | v :: s, e, t, Bind ident :: p -> 
    let e' = update_env e ident v in
    (s, e', t, p)
  (*Fetch*)
  | s, e, t, Fetch ident :: p ->
    (match fetch_env e ident with
     | Some v -> (v :: s, e, t, p)
     | None -> panic (s, e, t, p) ("unbound identifier: " ^ ident))
  (* Less Than *)
  | Const (Num x) :: Const (Num y) :: s, e, t, Lt :: p -> Const (Bool (x < y)) :: s, e, t, p
  | Const (Num _) :: _, _, _, Lt :: _ -> panic c "type error (lt on non-integer or empty stack)"
  | _, _, _, Lt :: _ -> panic c "stack underflow (lt on single or empty)"
  (* Equals *)
    | Const (Num x) :: Const (Num y) :: s, e, t, Eq :: p -> Const (Bool (x = y)) :: s, e, t, p
    | Const (Bool x) :: Const (Bool y) :: s, e, t, Eq :: p -> Const (Bool (x = y)) :: s, e, t, p
    | _ :: _ :: _, _, _, Eq :: _ -> panic c "type error (eq on non-integer or non-boolean)"
    | _ :: [], _, _, Eq :: _ -> panic c "stack underflow (eq on single)"
    | [], _, _, Eq :: _ -> panic c "stack underflow (eq on empty)"
  (*function defition*)
  | s, e, t, Fun prog :: p -> 
    let closure = Clos { def_id = local_id e; captured = []; prog } in
    (closure :: s, e, t, p)

  | Clos {def_id;captured;prog}::s, e, t, Call::p -> 
      let loc_id = local_id e +1 in 
      let new_rec = {id = def_id;local=captured;called_def_id = loc_id ;return_prog = prog} in 
      s, Local(new_rec,e), t, prog@p 

  | _, _, _, Call :: _ -> panic c "Call command without closure on the stack"
  (*Debug*)
    | s, e, t, Debug str::p -> s, e, str::t, p
  (*Return*)
  (* Handling Return when exactly one value is on the stack, which should be preserved *)
  | [x], Local ({ return_prog; _ }, parent_env), t, Return :: rest ->
      ([x], parent_env, t, rest)  (* Return to caller's context with the rest of the program *)

  (* Handling Return with no values on the stack; no value to return *)
  | [], Local ({ return_prog; _ }, parent_env), t, Return :: rest ->
      ([], parent_env, t, return_prog)  (* Continue with the caller's return program *)

  (* Handling the Return from a function at the end of a program sequence without further commands *)
  | s, Local ({ return_prog; _ }, parent_env), t, [] ->
      (s, parent_env, t, return_prog)  (* Continue execution in parent environment *)

  (* Error Handling for multiple values on the stack at Return: Error case *)
  | _ :: _ :: _, Local ({ return_prog; _ }, _), t, Return :: _ ->
      panic c "Return with multiple values on the stack is not allowed"

  (* Attempt to return in a global context which is not permissible *)
  | _, Global _, t, Return :: _ ->
      panic c "Return command executed in global context"


  (* Catch all for other unmatched cases *)
  | _, _, _, _ -> panic c "Unhandled command or type error"
  
  
let rec eval c =
  match c with
  | (_, Global _, t, []) -> t
  | _ -> eval (eval_step c)

let rec eval_prog p = eval ([], Global [], [], p)
let interp s = Option.map eval_prog (parse_prog s)

(* MAIN *)

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

(* END OF FILE *)
