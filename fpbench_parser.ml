open Num

(* BEGIN: Environment *)

type raw_expr =
  | Identifier of string
  | Numeral of num
  | Raw_rounding of Rounding.rnd_info * raw_expr
  | Raw_u_op of string * raw_expr
  | Raw_bin_op of string * raw_expr * raw_expr
  | Raw_gen_op of string * raw_expr list

type raw_formula =
  | Raw_le of raw_expr * raw_expr
  | Raw_lt of raw_expr * raw_expr
  | Raw_eq of raw_expr * raw_expr

  (* Prints a raw expression *)
let print_raw_expr fmt =
  let p = Format.pp_print_string fmt in
  let rec print = function
    | Identifier name -> p name
    | Numeral n -> p (string_of_num n)
    | Raw_rounding (rnd, e) ->
      p (Rounding.rounding_to_string rnd); p "("; print e; p ")";
    | Raw_u_op (op, e) -> 
      p op; p "("; print e; p ")";
    | Raw_bin_op (op, e1, e2) ->
      p op; p "("; print e1; p ","; print e2; p ")";
    | Raw_gen_op (op, es) ->
      p op; p "("; Lib.print_list print (fun () -> p ",") es; p ")";
  in
  print

let print_raw_expr_std = print_raw_expr Format.std_formatter

(* END: Environment *)

let print_raw_formula fmt = function
  | Raw_le (e1, e2) -> 
    Format.fprintf fmt "%a <= %a\n" print_raw_expr e1 print_raw_expr e2
  | Raw_lt (e1, e2) -> 
    Format.fprintf fmt "%a < %a\n" print_raw_expr e1 print_raw_expr e2
  | Raw_eq (e1, e2) -> 
    Format.fprintf fmt "%a == %a\n" print_raw_expr e1 print_raw_expr e2

let print_raw_formula_std = print_raw_formula Format.std_formatter

let dest_formula = function
  | Raw_le (e1, e2) -> "<=", e1, e2
  | Raw_lt (e1, e2) -> "<", e1, e2
  | Raw_eq (e1, e2) -> "==", e1, e2

let rec get_vars = function
  | Identifier id -> [id]
  | Numeral _ -> []
  | Raw_rounding (_, e) -> get_vars e
  | Raw_u_op (_, e) -> get_vars e
  | Raw_bin_op (_, e1, e2) -> get_vars e1 @ get_vars e2
  | Raw_gen_op (_, es) -> List.concat (List.map get_vars es)

type raw_bool_expr =
  | AND of raw_bool_expr list
  | OR of raw_bool_expr list
  | NOT of raw_bool_expr
  | CMP of string * raw_expr * raw_expr

let is_and expr = match expr with AND _ -> true | _ -> false

let is_or expr = match expr with OR _ -> true | _ -> false

let simplify_bool_expr expr =
  let rec remove_neg neg_flag expr =
    match expr with
    | AND args -> 
      let args' = List.map (remove_neg neg_flag) args in
      if neg_flag then OR args' else AND args'
    | OR args ->
      let args' = List.map (remove_neg neg_flag) args in
      if neg_flag then AND args' else OR args'
    | NOT arg -> 
      if neg_flag then remove_neg false arg else remove_neg true arg
    | CMP (op, arg1, arg2) ->
      if neg_flag then
        let new_op = 
          match op with
          | "<" -> ">="
          | ">" -> "<="
          | "<=" -> ">"
          | ">=" -> "<"
          | "==" -> "!="
          | "!=" -> "=="
          | _ -> failwith ("simplify_bool_expr: unknown comparison operator: " ^ op) in
        CMP (new_op, arg1, arg2)
      else
        expr in
  remove_neg false expr

let rec bool_expr_to_formulas expr =
  let expr = simplify_bool_expr expr in
  match expr with
  | OR _ -> failwith "Not supported: or"
  | NOT _ -> failwith "Not supported: not"
  | AND args ->
    let conjs = List.map bool_expr_to_formulas args in
    List.concat conjs
  | CMP (op, arg1, arg2) -> begin
    match op with
    | "<" -> [Raw_lt (arg1, arg2)]
    | ">" -> [Raw_lt (arg2, arg1)]
    | "<=" -> [Raw_le (arg1, arg2)]
    | ">=" -> [Raw_le (arg2, arg1)]
    | "==" -> [Raw_eq (arg1, arg2)]
    | _ -> failwith ("Unsupported comparison operator: " ^ op)
  end

let rec all_pairs =
  let rec pairs x = function
    | [] -> []
    | h :: t -> (x, h) :: pairs x t in
  function
  | [] -> []
  | x :: rest -> List.rev_append (pairs x rest) (all_pairs rest)

let rec ordered_pairs = function
  | h1 :: ((h2 :: t) as rest) -> (h1, h2) :: ordered_pairs rest
  | _ -> []

let mk_cmp_ops op args =
  let pairs = if op = "!=" then all_pairs args else ordered_pairs args in
  AND (List.map (fun (a, b) -> CMP (op, a, b)) pairs)

type token =
  | EOF
  | COLON
  | SEMICOLON
  | LPAREN
  | RPAREN
  | LBRACKET
  | RBRACKET
  | NUMBER of (string)
  | SYMBOL of (string)
  | STRING of (string)

let string_of_token = function
| EOF -> "EOF"
| COLON -> ":"
| SEMICOLON -> ";"
| LPAREN -> "("
| RPAREN -> ")"
| LBRACKET -> "["
| RBRACKET -> "]"
| NUMBER str -> str
| SYMBOL str -> str
| STRING str -> str

exception Expected of string

exception Noparse of string

type parser_state = {
  tok : Lexing.lexbuf -> token;
  lexbuf : Lexing.lexbuf;
  mutable token : token option
}

let mk_state tok lexbuf = {
  tok = tok;
  lexbuf = lexbuf;
  token = None;
}

let next_token state =
  match state.token with
  | Some token -> 
    state.token <- None; 
    token
  | None ->
    let token = state.tok state.lexbuf in
    token

let peek_token state =
  match state.token with
  | Some token -> token
  | None -> 
    let token = state.tok state.lexbuf in
    state.token <- Some token;
    token

let pushback_token token state =
  match state.token with
  | None -> state.token <- Some token
  | _ -> failwith "pushback_token: state is not empty"

type s_expr = 
  | Number of Num.num 
  | Symbol of string 
  | String of string
  | List of s_expr list

let rec print_s_expr = function
  | Number n -> Printf.printf "%s " (Num.string_of_num n)
  | Symbol sym -> Printf.printf "%s " sym
  | String str -> Printf.printf "\"%s\" " str
  | List l -> Printf.printf "("; List.iter print_s_expr l; Printf.printf ") "

let rec parse_s_expr state =
  let token = next_token state in
  match token with
  | NUMBER str -> Number (More_num.num_of_float_string str)
  | SYMBOL sym -> Symbol sym
  | STRING str -> String str
  | LPAREN | LBRACKET -> pushback_token token state; parse_s_expr_list state
  | _ -> raise (Expected "NUMBER or SYMBOL or STRING or LIST")
and parse_s_expr_list state =
  let first_token = next_token state in
  if first_token <> LPAREN && first_token <> LBRACKET then raise (Expected "( or [");
  let result = ref [] in
  let flag = ref true in
  while !flag do
    let token = peek_token state in
    match token with
    | RBRACKET when first_token = LBRACKET -> 
      ignore (next_token state); flag := false
    | RPAREN when first_token = LPAREN -> 
      ignore (next_token state); flag := false
    | _ -> result := parse_s_expr state :: !result
  done;
  List (List.rev !result)


let mk_binop op args =
  match args with
  | [a1; a2] -> Raw_bin_op (op, a1, a2)
  | _ -> failwith "mk_binop: wrong number of arguments"

let mk_uop op args =
  match args with
  | [a1] -> Raw_u_op (op, a1)
  | _ -> failwith "mk_uop: wrong number of arguments"

let mk_genop op args = Raw_gen_op (op, args)

let mk_rnd rnd arg = Raw_rounding (rnd, arg)

let symbol s_expr =
  match s_expr with
  | Symbol sym -> sym
  | _ -> failwith "SYMBOL expected"

let head s_expr =
  match s_expr with
  | List (h :: t) -> h
  | _ -> failwith "Nonempty LIST expected"

let head_and_args s_expr =
  match s_expr with
  | List (Symbol h :: t) -> h, t
  | _ -> failwith "Nonempty LIST with SYMBOL head expected"

type data = {
  name : string;
  precision : Rounding.rnd_info;
  vars : (string * (raw_expr option * raw_expr option)) list;
  constraints : raw_formula list;
  expr : raw_expr;
}
  
let mk_data var_names = {
  name = "No_name";
  precision = Rounding.string_to_rounding "rnd64";
  vars = List.map (fun v -> (v, (None, None))) var_names;
  constraints = [];
  expr = Identifier "$"
}

let translate_var_or_const env sym =
  try List.assoc sym env
  with Not_found ->
    match sym with
    | "E" -> mk_uop "exp" [Numeral (Int 1)]
    | "2_PI" -> mk_binop "*" [Numeral (Int 8); mk_uop "atan" [Numeral (Int 1)]]
    | "PI" -> mk_binop "*" [Numeral (Int 4); mk_uop "atan" [Numeral (Int 1)]]
    | "PI_2" -> mk_binop "*" [Numeral (Int 2); mk_uop "atan" [Numeral (Int 1)]]
    | "PI_4" -> mk_uop "atan" [Numeral (Int 1)]
    | "SQRT2" -> mk_uop "sqrt" [Numeral (Int 2)]
    | "SQRT1_2" -> mk_uop "sqrt" [Numeral (Int 1 // Int 2)]
    | _ -> failwith ("Undefined variable: " ^ sym)



let rec translate_expr data env s_expr =
  match s_expr with
  | Number n -> Numeral n
  | Symbol sym -> translate_var_or_const env sym
  | List (Symbol head :: _) -> begin
    match head with
    | "if" -> failwith "Not supported: if"
    | "while" -> failwith "Not supported: while"
    | "let" -> translate_let data env s_expr
    | _ -> translate_op data env s_expr
  end
  | _ -> failwith "Bad EXPR"

and translate_let data env s_expr =
  let binding s_expr =
    match s_expr with
    | List [Symbol name; expr] -> name, translate_expr data env expr
    | _ -> failwith "Bad LET binding" in
  match s_expr with
  | List [Symbol "let"; List bindings; expr] ->
    let defs = List.map binding bindings in
    let env = List.fold_left (fun e d -> d :: e) env defs in
    translate_expr data env expr
  | _ -> failwith "Bad LET"

and translate_op data env s_expr =
  let op, args' = head_and_args s_expr in
  let args = List.map 
    (fun arg -> mk_rnd data.precision (translate_expr data env arg)) args' in
  let result = 
    match op with
    | "+" -> mk_binop "+" args
    | "-" when List.length args = 1 -> mk_uop "-" args
    | "-" -> mk_binop "-" args
    | "*" -> mk_binop "*" args
    | "/" -> mk_binop "/" args
    | "sqrt" -> mk_uop "sqrt" args
    | "fabs" -> mk_uop "abs" args
    | "exp" -> mk_uop "exp" args
    | "log" -> mk_uop "log" args
    | "sin" -> mk_uop "sin" args
    | "cos" -> mk_uop "cos" args
    | "tan" -> mk_uop "tan" args
    | "asin" -> mk_uop "asin" args
    | "acos" -> mk_uop "acos" args
    | "atan" -> mk_uop "atan" args
    | "fmin" -> mk_binop "min" args
    | "fmax" -> mk_binop "max" args
    | _ -> failwith ("Unsupported operation: " ^ op) in
  mk_rnd data.precision result

and translate_bool_expr data env s_expr =
  let op, args = head_and_args s_expr in
  match op, args with
  | "not", [arg] -> NOT (translate_bool_expr data env arg)
  | "or", _ -> OR (List.map (translate_bool_expr data env) args)
  | "and", _ -> AND (List.map (translate_bool_expr data env) args)
  | cmp_op, _ ->
    let args' = List.map (translate_expr data env) args in
    mk_cmp_ops cmp_op args'

let rec replace_assoc key v = function
  | [] -> raise Not_found
  | (k, _) :: t when k = key -> (k, v) :: t
  | h :: t -> h :: replace_assoc key v t

(* TODO: a constraint propagation algorithm to resolve bounds like 1 <= x <= y <= 2 *)
let add_var_bounds data formula =
  let cmp_op, e1, e2 = dest_formula formula in
  let var, (lo, hi) =
    match (e1, e2) with
    | (Identifier x, _) when get_vars e2 = [] ->
      let lo, hi = match cmp_op with
                   | "==" -> Some e2, Some e2
                   | "<" | "<=" -> None, Some e2
                   | _ -> failwith "impossible" in
      
      x, (lo, hi)
    | (_, Identifier x) when get_vars e1 = [] ->
      let lo, hi = match cmp_op with
                   | "==" -> Some e1, Some e1
                   | "<" | "<=" -> Some e1, None
                   | _ -> failwith "impossible" in
      x, (lo, hi)
    | _ -> failwith "Bad constraint" in
  let lo1, hi1 = List.assoc var data.vars in
  let lo = if lo = None then lo1 else lo in
  let hi = if hi = None then hi1 else hi in
  { data with vars = replace_assoc var (lo, hi) data.vars }

let is_var_bound_formula f =
  let _, e1, e2 = dest_formula f in
  match (e1, e2) with
  | (Identifier _, _) when get_vars e2 = [] -> true
  | (_, Identifier _) when get_vars e1 = [] -> true
  | _ -> false

let translate_pre data env s_expr =
  let formulas = bool_expr_to_formulas (translate_bool_expr data env s_expr) in
  let var_bounds, constraints = List.partition is_var_bound_formula formulas in 
  let data = List.fold_left (fun d v -> add_var_bounds d v) data var_bounds in
  { data with constraints = constraints }

let translate_precision prec =
  match prec with
  | "binary32" -> Rounding.string_to_rounding "rnd32"
  | "binary64" -> Rounding.string_to_rounding "rnd64"
  | _ -> failwith ("Unsupported precision: " ^ prec)

let rec translate_properties data env s_exprs =
  match s_exprs with
  | Symbol name :: arg :: rest when Lib.starts_with name ~prefix:":" ->
    let data = begin
      match (name, arg) with
      | ":name", String str -> 
        { data with name = str }
      | ":precision", Symbol prec -> 
        { data with precision = translate_precision prec }
      | ":pre", _ -> translate_pre data env arg
      (* Skip unknown properties *)
      | _ -> data
    end in
    translate_properties data env rest
  | _ -> data, s_exprs

let translate_fpcore s_expr =
  match s_expr with
  | List (Symbol "FPCore" :: List vars :: rest) ->
    let var_names = List.map symbol vars in
    let data = mk_data var_names in
    let env = List.map (fun v -> v, Identifier v) var_names in
    let data, rest = translate_properties data env rest in begin
      match rest with
      | [s_expr] -> { data with expr = translate_expr data env s_expr }
      | _ -> failwith "EXPR expected"
    end
  | _ -> failwith "FPCore expected"

