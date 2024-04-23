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
  | Add | Mul | Div |Sub(*new*)
  | And | Or | Not | Lt | Eq
  | If of program * program
  | While of program * program
  | Bind of ident | Fetch of ident
  | Fun of ident * program | Call of ident | Return
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

let parse_comment =
  char '(' >> many (satisfy ((<>) ')')) >> char ')' >| ()

let parse_debug =
  char '"' >> many (satisfy ((<>) '"')) << char '"' >|= implode

let parse_const =
  let parse_num = parse_int >|= fun n -> Num n in  (* Assume Num n is of type const *)
  let parse_bool = parse_bool >|= fun b -> Bool b in  (* Assume Bool b is of type const *)
  choice [parse_num; parse_bool]

let parse_push =
    keyword "Push" >>
    parse_const >|=
    (fun v -> Push v)

let parse_bind =
      keyword "|>" >>
      parse_ident >|=
      (fun x -> Bind x)

let parse_trace =
    keyword "Trace" >> pure Trace

let parse_return =
      keyword "Return" >> pure Return
    
let parse_call =
   keyword "#" >> parse_ident >|= (fun name -> Call name)

let parse_add =
    keyword "Add" >> pure Add

let parse_mul =
    keyword "Mul" >> pure Mul
  
let parse_div =
    keyword "Div" >> pure Div
  
let parse_sub =
    keyword "Sub" >> pure Sub
  
let parse_and =
    keyword "And" >> pure And
  
let parse_or =
    keyword "Or" >> pure Or
  
let parse_not =
    keyword "Not" >> pure Not
  
let parse_lt =
    keyword "Lt" >> pure Lt
  
let parse_eq =
    keyword "Eq" >> pure Eq

(* between: Parses content between two delimiters using provided parsers for open, close, and content. *)
let between open_parser close_parser content_parser input =
  match open_parser input with
  | None -> None
  | Some (_, rest1) ->  (* Successfully parsed the opening delimiter *)
      match content_parser rest1 with
      | None -> None
      | Some (content, rest2) ->  (* Successfully parsed the content *)
          match close_parser rest2 with
          | None -> None
          | Some (_, rest3) ->  (* Successfully parsed the closing delimiter *)
              Some (content, rest3)

  
let parse_block =
      between (char '(') (char ')') (many parse_int)
    
   

let ws = many (ws >> parse_comment) >> ws
let keyword w = str w << ws

let rec parse_com () =
  let parse_fun =
    let* _ = keyword "def" in
    let* name = parse_ident in
    let* _ = keyword "|" in
    let* _ = keyword ">" in
    let* prog = parse_prog_rec () in
    let* _ = keyword "end" in
    let* _ = char '(' in
    let* name = parse_ident in
    let* _ = char ')' in
    let* body = parse_block in
    pure (Fun(name, prog))
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
    [ parse_push 
    ; parse_bind 
    ; parse_fun 
    ; parse_while 
    ; parse_if 
    ; parse_trace
    ; parse_return
    ; parse_add
    ; parse_mul
    ; parse_div
    ; parse_sub
    ; parse_and
    ; parse_or
    ; parse_not
    ; parse_lt
    ; parse_eq
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
| Global bindings -> List.assoc_opt x bindings (* Search in global bindings *)
| Local (record, enclosing_env) -> 
  (match List.assoc_opt x record.local with
  | Some _ as result -> result (* Found in local bindings *)
  | None -> fetch_env enclosing_env x) (* Search in enclosing environment *)

let rec update_env e x v = (* TODO *)
match e with
| Global bindings -> Global ((x, v) :: bindings) (* Update global bindings *)
| Local (record, enclosing_env) ->
  if List.exists (fun (var, _) -> var = x) record.local then
    (* Update existing local binding *)
    let updated_locals = List.map (fun (var, value) -> if var = x then (x, v) else (var, value)) record.local in
    Local ({record with local = updated_locals}, enclosing_env)
  else
    (* If not found in local, update enclosing environment *)
    let updated_enclosing = update_env enclosing_env x v in
    Local (record, updated_enclosing)

(* EVALUTION *)

(* make the panic configuration given a configuration *)
let panic (_, _, t, _) msg = [], Global [], ("panic: " ^ msg) :: t, []

let eval_step (c : stack * env * trace * program) =
  match c with
  (* Push *)
  | s, e, t, Push c :: p -> Const c :: s, e, t, p
  (* Trace *)
  | v :: s, e, t, Trace :: p -> s, e, to_string v :: t, p
  | [], _, _, Trace :: _ -> panic c "stack underflow (. on empty)"
  (* Add *)
  | Const (Num m) :: Const (Num n) :: s, e, t, Add :: p -> Const (Num (m + n)) :: s, e, t, p
  | _ :: _ :: _, _, _, Add :: _ -> panic c "type error (+ on non-integers)"
  | _ :: [], _, _, Add :: _ -> panic c "stack underflow (+ on single)"
  | [], _, _, Add :: _ -> panic c "stack underflow (+ on empty)"
  (* TODO *)
  (* Subtraction *)
  | Const (Num m) :: Const (Num n) :: s, e, t, Sub :: p -> Const (Num (m - n)) :: s, e, t, p
  (* Implement more arithmetic operations here *)
  | Const (Num m) :: Const (Num n) :: s, e, t, Mul :: p -> Const (Num (m * n)) :: s, e, t, p
  | Const (Num m) :: Const (Num n) :: s, e, t, Div :: p when n != 0 -> Const (Num (m / n)) :: s, e, t, p
  | Const (Num _) :: Const (Num 0) :: _, _, _, Div :: _ -> panic c "division by zero"
  (* Logical operations *)
  | Const (Bool b1) :: Const (Bool b2) :: s, e, t, And :: p -> Const (Bool (b1 && b2)) :: s, e, t, p
  | Const (Bool b1) :: Const (Bool b2) :: s, e, t, Or :: p -> Const (Bool (b1 || b2)) :: s, e, t, p
  | Const (Bool b) :: s, e, t, Not :: p -> Const (Bool (not b)) :: s, e, t, p
  (* If, While, and other control structures *)
  | Const (Bool true) :: s, e, t, If (then_p, _) :: p -> s, e, t, then_p @ p
  | Const (Bool false) :: s, e, t, If (_, else_p) :: p -> s, e, t, else_p @ p
  | _ :: s, e, t, While (cond_p, body_p) :: p -> s, e, t, If (cond_p, body_p @ [While (cond_p, body_p)]) :: p
  (* Error and unhandled cases *)
  | _, _, _, _ -> panic c "unhandled command or type error"

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
