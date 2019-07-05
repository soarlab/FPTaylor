open Printf
  


let rec str2listchar st = match st with
    | "" -> []
    | st -> (String.make 1 (String.get st 0) ) :: (str2listchar (String.sub st 1 ( (String.length st)-1) ) )  ;;

let ch2int c = match c with
    | "0" ->  0
    | "1" ->  1
    | "2" ->  2
    | "3" ->  3
    | "4" ->  4
    | "5" ->  5
    | "6" ->  6
    | "7" ->  7
    | "8" ->  8
    | "9" ->  9 
    | c -> -1 ;;


let rec extint st =  List.filter (fun x-> x>=0) (List.map ch2int (str2listchar st));;

let rec getint l s =  if List.length l > 0 then getint (List.tl l) (s*10+ List.hd l) else s;;

let rec getintmax l s m =  if (List.length l > 0)&&(s < m) then getintmax (List.tl l) (s*10+ List.hd l) m else s;;

let getfirstint st = let lint = extint st in getint lint 0 ;;

let getfirstintmax st = let lint = extint st in getintmax lint 0 300;;

let read_file filename = 
	let lines = ref [] in
	let chan = open_in filename in
	try
 	 while true; do
  	  lines := input_line chan :: !lines
 	 done; !lines
	with End_of_file ->
 	 close_in chan;
 	 List.rev !lines ;;

let rec addline strl1 strl2 seek s= 
	if (String.compare (List.hd strl1) seek)==0 then s@[seek]@strl2@(List.tl strl1)
	else addline (List.tl strl1) strl2 seek (s@[List.hd strl1]);;

let rec addlineall strl1 strl2 seek s= if (List.length strl1) ==0 then s else
	if (String.compare (List.hd strl1) seek)==0 then addlineall (List.tl strl1) strl2 seek s@[seek]@strl2
	else addlineall (List.tl strl1) strl2 seek (s@[List.hd strl1]);;

let rec addlinebefore strl1 strl2 seek s= 
	if (String.compare (List.hd strl1) seek)==0 then s@strl2@[seek]@(List.tl strl1)
	else addlinebefore (List.tl strl1) strl2 seek (s@[List.hd strl1]);;

let rec getlinebefore strl1 seek= 
	if (String.compare (List.hd (List.tl strl1)) seek)==0 then List.hd strl1
	else getlinebefore (List.tl strl1) seek;;

let rec formula_to_code str =	
	let binop = ["+";"-";"*";"/";"^"] in
	let unfun = ["-";"abs";"inv";"sqrt";"sin";"cos";"tan";"asin";"acos";"atan"] in
	let binfun=["max";"min"] in
	let opname st = match st with | "+" -> "add" | "-" -> "sub"| "*" -> "mul" | "/" -> "div" | "^" -> "nat_pow" in
	let opval st = match st with | "+" -> 1 | "-" -> 1 | "*" -> 2 | "/" -> 2 | "^" -> 3 in
	let inlist l e = List.exists (fun x -> ((String.compare x e)==0) ) l in 
	let rec countparen st l r= match st with
   		 | "" -> (l,r)
   		 | st -> if (String.compare (String.make 1 (String.get st 0) ) "(" ) ==0 then  countparen  (String.sub st 1 ( (String.length st)-1) )  (l+1) r
			else if (String.compare (String.make 1 (String.get st 0) ) ")" ) ==0 then  countparen  (String.sub st 1 ( (String.length st)-1) )  l (r+1)
			else countparen  (String.sub st 1 ( (String.length st)-1) )  l r in

	let balance st = let (l,r) = countparen st 0 0 in (l==r) in

	let rec redundantparen st c = if c+1 == String.length st then (balance st) else let (l,r) = countparen( String.sub st 0 c ) 0 0 in 
													if r>=l then false else redundantparen st (c+1) in
											      
	let lastchar st = let l = String.length st in String.sub st (l-1) 1 in
	let removeparen st = if String.length st == 0 then st else  if redundantparen st 1 &&  
								((String.compare (String.sub st 0 1) "(" ) ==0) &&
								((String.compare (lastchar st) ")" )==0) then (String.sub st 1 ( (String.length st)-2) ) else st in
	let rec removeallparen st = if String.compare st (removeparen st) == 0 then st else removeallparen (removeparen st) in
	let rec removespace st= if (String.contains st ' ')==false then st 
				else let c = (String.index st ' ') in let stleft=String.sub st 0 c in let stright=String.sub st (c+1) ((String.length st)-c-1) in String.concat "" [stleft;stright] in
	let rec removespaces st = if String.compare st (removespace st) == 0 then st else removeallparen (removespace st) in
	let rec standard st = removeallparen (removespaces st) in
	let rec find_bin_op st c m t= if ((c==((String.length st)-1)) && (m==5)) || ((String.length st)<2) then ("No","","") else
				  if ((c==((String.length st)-1)) && (m<5)) then t else
				  let stleft=String.sub st 0 c in 
				  let stright=String.sub st (c+1) ((String.length st)-c-1) in
				  let op=String.make 1 (String.get st c) in
				  if (inlist binop op) && (balance stleft) && (balance stright) && (m>opval op) then find_bin_op st (c+1) (opval op) (op,stleft,stright) else find_bin_op st (c+1) m t in

	let rec splitexpr st c = if (c==((String.length st)-1)) || ((String.length st)<2)  then ("","") else
				  let stleft=String.sub st 0 c in 
				  let stright=String.sub st (c+1) ((String.length st)-c-1) in
				  let com=String.make 1 (String.get st c) in
				  if ((String.compare com ",") == 0) && (balance stleft) && (balance stright) then (stleft,stright) else splitexpr st (c+1) in

	let rec find_bin_func st   = if (String.contains st '(')==false then ("No","","") else let c = (String.index st '(') in 
				     let op = String.sub st 0 c in 
				     let inexpr = String.sub st (c+1) ((String.length st)-c-2) in
				     let (lexpr,rexpr) = splitexpr inexpr 1 in
					if ((String.compare lexpr "") == 0) || ((inlist binfun op)==false) then ("No","","") else  (op,lexpr,rexpr) in

	let rec find_un_func st   = if (String.contains st '(')==false then ("No","") else let c = (String.index st '(') in 
				     let op = String.sub st 0 c in 
				     let inexpr = String.sub st (c+1) ((String.length st)-c-2) in
					if ((inlist unfun op)==false) then ("No","") else if (String.compare op "-") == 0 then ("neg",inexpr) else (op,inexpr) in	

		 
	let expr = standard str in
		let (op,l,r) = find_bin_op expr 1 5 ("No","","") in 
		if (String.compare op "No" <> 0) then String.concat "" ["(mk_"; (opname op) ; " "; formula_to_code l; " "; formula_to_code r; ")"] else
		let (op,l,r) = find_bin_func expr  in 
		if (String.compare op "No" <> 0) then String.concat "" ["(mk"; op ; " ";  formula_to_code l; " " ; formula_to_code r; ")"] else
		let (op,l) = find_un_func expr in 
		if (String.compare op "No" <> 0) then String.concat "" ["(mk_"; op ; " "; formula_to_code l; ")"] else
		if String.compare expr "x" == 0 then "x" else 
		if String.compare expr "-x" == 0 then "(mk_neg x)" else 
		if String.contains expr '.' then String.concat "" ["(mk_const (Const.of_float (";expr;  ")))"] else
 						String.concat "" ["(mk_const (Const.of_float (";expr;  ".0)))"] ;;

let write_taylor_form_ml_dev funcname first_derivative second_derivative_formula output=
	let inlines = read_file "taylor_form.ml" in
	let code = [
(String.concat "" ["let "; funcname; "_form cs f ="] );
"  let x0_int = estimate_expr cs f.v0 in";
"  let x1 = abs_eval_v1 cs f.v1 in";
"  let s1 = Lib.itlist (fun (x,x_exp) s -> ";
"      let eps = get_eps x_exp in";
"      let xi = {low = -. eps; high = eps} in";
"      (xi *$. x) +$ s) x1 zero_I in";
"  let m1 = make_stronger (abs_I s1).high in";
"  let d =";
"    let x =";
"      if Config.proof_flag then";
"        x0_int +$ {low = -.m1; high = m1}";
"      else";
"        x0_int +$ s1 in";
(String.concat "" ["    ";second_derivative_formula;"  in"]);
"  let b_high = 0.5 *^ (abs_I d).high in";
"  let b_high = make_stronger b_high in";
"  let m2, m2_exp = sum2_high x1 x1 in";
"  let m2 = make_stronger m2 in";
"  let m3 = b_high *^ m2 in";
"  let form_index = next_form_index() in";
"  let m3_err = mk_err_var (-1) m2_exp in";
"  let x = f.v0 in";
"  {";
"    form_index = form_index;";
(String.concat "" ["    v0 = mk_"; funcname; " f.v0;"]);
(String.concat "" ["    v1 = List.map (fun (e, err) -> (mk_mul e "; formula_to_code first_derivative ;", err)) f.v1"]);
"         @ [mk_float_const m3, m3_err];";
"  }"] in
	
	let codepre = "(*new function*)" in	
	let lines = addline inlines code codepre [] in

	let code = [String.concat "" ["        | Op_";funcname; " -> "; funcname; "_form cs arg_form"] ] in
	let codepre = "        | Op_atanh -> atanh_form cs arg_form" in	
	let lines = addline lines code codepre [] in

	let chan = open_out output in
	List.iter (fprintf chan "%s\n") lines;
	close_out chan;;

let write_taylor_form_ml_code funcname incode output=
	let lines = read_file "taylor_form.ml" in
	let code = incode in
	let codepre = "(* Builds a Taylor form *)" in	
	let lines = addlinebefore lines code codepre [] in

	let code = [String.concat "" ["        | Op_";funcname; " -> "; funcname; "_form cs arg_form"] ] in
	let codepre = "        | Op_atanh -> atanh_form cs arg_form" in	
	let lines = addline lines code codepre [] in

	let chan = open_out output in
	List.iter (fprintf chan "%s\n") lines;
	close_out chan;;


let write_expr_ml funcname output=
	let lines = read_file "expr.ml" in
	let code = [String.concat "" ["  | Op_";funcname] ] in
	let codepre = "  | Op_atanh" in	
	let lines = addline lines code codepre [] in

	let code = [String.concat "" ["  mk_";funcname; " a = U_op (Op_"; funcname; ", a) and"] ] in
	let codepre = "  mk_atanh a = U_op (Op_atanh, a) and" in	
	let lines = addline lines code codepre [] in

	let code = [String.concat "" ["  | Op_"; funcname; " -> \"";funcname; "\""] ] in
	let codepre = "  | Op_atanh -> \"atanh\"" in	
	let lines = addline lines code codepre [] in

	let chan = open_out output in
	List.iter (fprintf chan "%s\n") lines;
	close_out chan;;

let write_expr_mli funcname output=
	let lines = read_file "expr.mli" in
	let code = [String.concat "" ["  | Op_";funcname] ] in
	let codepre = "  | Op_atanh" in	
	let lines = addline lines code codepre [] in

	let code = [String.concat "" ["val mk_";funcname; " : expr -> expr"] ] in
	let codepre = "val mk_atanh : expr -> expr" in	
	let lines = addline lines code codepre [] in
	
	let chan = open_out output in
	List.iter (fprintf chan "%s\n") lines;
	close_out chan;;

let write_eval_ml funcname floatexpr numexpr intervalexpr output=
	let lines = read_file "eval.ml" in
	let code = [String.concat "" ["        | Op_";funcname;" -> "; floatexpr] ] in
	let codepre = "        | Op_atanh -> Func.atanh x" in	
	let lines = addline lines code codepre [] in
	
	let code = [String.concat "" ["        | Op_";funcname;" -> "; numexpr] ] in
	let codepre = "        | Op_inv -> one // x" in	
	let lines = addline lines code codepre [] in

	let code = [String.concat "" ["        | Op_";funcname;" -> "; intervalexpr] ] in
	let codepre = "        | Op_atanh -> Func.atanh_I x" in	
	let lines = addline lines code codepre [] in

	let chan = open_out output in
	List.iter (fprintf chan "%s\n") lines;
	close_out chan;;

let write_exprOut_ml funcname intervalexpr output=
	let lines = read_file "exprOut.ml" in
	let code = [String.concat "" ["        | Op_";funcname;" -> fprintf fmt \""; funcname;"(%a)\" print arg"] ] in
	let codepre = "        | Op_neg -> fprintf fmt \"(-(%a))\" print arg" in	
	let lines = addline lines code codepre [] in
	
	let tok e = List.exists (fun x -> ((String.compare x e)==0) )   ["(";")";"+";"-";"*";"/";"^";" ";",";".";"~";"$"] in 
	let replace str = 
		let rec findx st c = let l = (String.length st) in if c==l then -1 else
				     if ((c==0) && (0==String.compare (String.sub st 0 1) "x") && (tok (String.sub st 1 1))) ||
					((c==l-1) && (0==String.compare (String.sub st (l-1) 1) "x") && (tok (String.sub st (l-2) 1))) ||
					((0 == String.compare (String.sub st c 1) "x") && (tok (String.sub st (c-1) 1))&& (tok (String.sub st (c+1) 1))) then c
					else findx st (c+1) in
		let c =findx str 0 in 
			if c == -1 then str else String.concat "" [(String.sub str 0 c); "(%a)"; (String.sub str (c+1) ((String.length str)-c-1) )]  in
	
	let rec replaceall str = if 0==String.compare str (replace str) then str else replaceall (replace str) in


	let code = [String.concat "" ["        | Op_";funcname;" -> fprintf fmt \""; replaceall intervalexpr;"\" print arg"] ] in
	let codepre = "        | Op_atanh -> fprintf fmt \"atanh_I(%a)\" print arg" in	
	let lines = addline lines code codepre [] in


	let code = [String.concat "" ["        | Op_";funcname;" -> fprintf fmt \"("; funcname;" %a)\" print arg"] ] in
	let codepre = "        | Op_neg -> fprintf fmt \"(- %a)\" print arg" in	
	let lines = addline lines code codepre [] in

	let code = [String.concat "" ["        | Op_";funcname;" -> fprintf fmt \"(i"; funcname;" %a)\" print arg"] ] in
	let codepre = "        | Op_neg -> fprintf fmt \"(i- %a)\" print arg" in	
	let lines = addline lines code codepre [] in

	let code = [String.concat "" ["        | Op_";funcname;" -> fprintf fmt \""; funcname;"(%a)\" print arg"] ] in
	let codepre = "        | Op_neg -> fprintf fmt \"(-.(%a))\" print arg" in	
	let lines = addline lines code codepre [] in

	let chan = open_out output in
	List.iter (fprintf chan "%s\n") lines;
	close_out chan;;

let write_input_lexer_ml funcname output=
	let lines = read_file "input_lexer.ml" in
	let code = [String.concat "" ["      \"";funcname;"\", "; (String.uppercase funcname);";"] ] in
	let codepre = "      \"argtanh\", ATANH;" in	
	let lines = addline lines code codepre [] in


	let chan = open_out output in
	List.iter (fprintf chan "%s\n") lines;
	close_out chan;;

let write_input_parser_mly funcname output=
	let lines = read_file "input_parser.mly" in
	let code = [String.concat "" ["%token "; (String.uppercase funcname)] ] in
	let codepre = "%token COS SIN TAN COSH SINH TANH" in	
	let lines = addline lines code codepre [] in

	let code = [String.concat "" ["  | ";(String.uppercase funcname);" LPAREN expr RPAREN { Raw_u_op (\""; funcname;"\", $3) }"] ] in
	let codepre = "  | ATANH LPAREN expr RPAREN { Raw_u_op (\"atanh\", $3) }" in	
	let lines = addline lines code codepre [] in

	let chan = open_out output in
	List.iter (fprintf chan "%s\n") lines;
	close_out chan;;

let write_input_lexer_mll funcname output=
	let lines = read_file "input_lexer.mll" in
	let code = [String.concat "" ["      \""; funcname ; "\", "; (String.uppercase funcname); ";"] ] in
	let codepre = "      \"argtanh\", ATANH;" in	
	let lines = addline lines code codepre [] in

	let chan = open_out output in
	List.iter (fprintf chan "%s\n") lines;
	close_out chan;;

let write_input_parser_env_ml funcname output=
	let lines = read_file "input_parser_env.ml" in
	let code = [String.concat "" ["      | \""; funcname ; "\" -> U_op (Op_"; funcname; ", e1)"] ] in
	let codepre = "      | \"tanh\" -> U_op (Op_tanh, e1)" in	
	let lines = addline lines code codepre [] in

	let chan = open_out output in
	List.iter (fprintf chan "%s\n") lines;
	close_out chan;;

let rec cover_to_code str =	
	let binop = ["+";"-";"*";"/";"^"] in
	let unfun = ["-";"abs";"inv";"sqrt";"sin";"cos";"tan";"asin";"acos";"atan"] in
	let binfun=["max";"min"] in
	let opname st = match st with | "+" -> "Op_add" | "-" -> "Op_sub"| "*" -> "Op_mul" | "/" -> "Op_div" | "^" -> "Op_nat_pow" in
	let opval st = match st with | "+" -> 1 | "-" -> 1 | "*" -> 2 | "/" -> 2 | "^" -> 3 in
	let inlist l e = List.exists (fun x -> ((String.compare x e)==0) ) l in 
	let rec countparen st l r= match st with
   		 | "" -> (l,r)
   		 | st -> if (String.compare (String.make 1 (String.get st 0) ) "(" ) ==0 then  countparen  (String.sub st 1 ( (String.length st)-1) )  (l+1) r
			else if (String.compare (String.make 1 (String.get st 0) ) ")" ) ==0 then  countparen  (String.sub st 1 ( (String.length st)-1) )  l (r+1)
			else countparen  (String.sub st 1 ( (String.length st)-1) )  l r in

	let balance st = let (l,r) = countparen st 0 0 in (l==r) in

	let rec redundantparen st c = if c+1 == String.length st then (balance st) else let (l,r) = countparen( String.sub st 0 c ) 0 0 in 
													if r>=l then false else redundantparen st (c+1) in
											      
	let lastchar st = let l = String.length st in String.sub st (l-1) 1 in
	let removeparen st = if String.length st == 0 then st else  if redundantparen st 1 &&  
								((String.compare (String.sub st 0 1) "(" ) ==0) &&
								((String.compare (lastchar st) ")" )==0) then (String.sub st 1 ( (String.length st)-2) ) else st in
	let rec removeallparen st = if String.compare st (removeparen st) == 0 then st else removeallparen (removeparen st) in
	let rec removespace st= if (String.contains st ' ')==false then st 
				else let c = (String.index st ' ') in let stleft=String.sub st 0 c in let stright=String.sub st (c+1) ((String.length st)-c-1) in String.concat "" [stleft;stright] in
	let rec removespaces st = if String.compare st (removespace st) == 0 then st else removeallparen (removespace st) in
	let rec standard st = removeallparen (removespaces st) in
	let rec find_bin_op st c m t= if ((c==((String.length st)-1)) && (m==5)) || ((String.length st)<2) then ("No","","") else
				  if ((c==((String.length st)-1)) && (m<5)) then t else
				  let stleft=String.sub st 0 c in 
				  let stright=String.sub st (c+1) ((String.length st)-c-1) in
				  let op=String.make 1 (String.get st c) in
				  if (inlist binop op) && (balance stleft) && (balance stright) && (m>opval op) then find_bin_op st (c+1) (opval op) (op,stleft,stright) else find_bin_op st (c+1) m t in

	let rec splitexpr st c = if (c==((String.length st)-1)) || ((String.length st)<2)  then ("","") else
				  let stleft=String.sub st 0 c in 
				  let stright=String.sub st (c+1) ((String.length st)-c-1) in
				  let com=String.make 1 (String.get st c) in
				  if ((String.compare com ",") == 0) && (balance stleft) && (balance stright) then (stleft,stright) else splitexpr st (c+1) in

	let rec find_bin_func st   = if (String.contains st '(')==false then ("No","","") else let c = (String.index st '(') in 
				     let op = String.sub st 0 c in 
				     let inexpr = String.sub st (c+1) ((String.length st)-c-2) in
				     let (lexpr,rexpr) = splitexpr inexpr 1 in
					if ((String.compare lexpr "") == 0) || ((inlist binfun op)==false) then ("No","","") else  (op,lexpr,rexpr) in

	let rec find_un_func st   = if (String.contains st '(')==false then ("No","") else let c = (String.index st '(') in 
				     let op = String.sub st 0 c in 
				     let inexpr = String.sub st (c+1) ((String.length st)-c-2) in
					if ((inlist unfun op)==false) then ("No","") else if (String.compare op "-") == 0 then ("neg",inexpr) else (op,inexpr) in	

		 
	let expr = standard str in
		let (op,l,r) = find_bin_op expr 1 5 ("No","","") in 
		if (String.compare op "No" <> 0) then String.concat "" ["Bin_op ("; (opname op) ; ","; cover_to_code l; ","; cover_to_code r; ")"] else
		let (op,l,r) = find_bin_func expr  in 
		if (String.compare op "No" <> 0) then String.concat "" ["Bin_op (Op_"; op ; ",";  cover_to_code l; "," ; cover_to_code r; ")"] else
		let (op,l) = find_un_func expr in 
		if (String.compare op "No" <> 0) then String.concat "" ["U_op (Op_"; op ; ","; cover_to_code l; ")"] else
		if String.compare expr "x" == 0 then "e1" else 
		if String.compare expr "-x" == 0 then "U_op (Op_neg, e1)" else 
		if String.contains expr '.' then String.concat "" ["mk_const (Const.of_float ";expr;  ")"] else
 						String.concat "" ["mk_const (Const.of_float ";expr;  ".0)"] ;;
		 


let write_input_parser_env_ml_code funcname incode output=
	let lines = read_file "input_parser_env.ml" in
	let code = [String.concat "" ["      | \""; funcname ; "\" -> "; cover_to_code incode] ] in
	let codepre = "      | \"tanh\" -> U_op (Op_tanh, e1)" in	
	let lines = addline lines code codepre [] in

	let chan = open_out output in
	List.iter (fprintf chan "%s\n") lines;
	close_out chan;;

let write_rounding_simpl_ml funcname intervalexpr output=
	let lines = read_file "rounding_simpl.ml" in
	let code = [String.concat "" ["          | Op_"; funcname ; " -> "; intervalexpr] ] in
	let codepre = "          | Op_atan -> atan_I x" in	
	let lines = addline lines code codepre [] in

	let chan = open_out output in
	List.iter (fprintf chan "%s\n") lines;
	close_out chan;;

let rec get2int_input_parser strl a b = if List.length strl == 0 then (string_of_int (a+1), string_of_int (b+7)) else 
			let n = getfirstint (List.hd strl) in if (n>200)&&(n<500) then get2int_input_parser (List.tl strl) n b else
							     if  (n>1000) then  get2int_input_parser (List.tl strl) a n  else
								get2int_input_parser (List.tl strl) a b;;

let write_input_parser_ml funcname output=
	let lines = read_file "input_parser.ml" in
	let code = [String.concat "" ["  | "; (String.uppercase funcname)] ] in
	let codepre = "  | ATANH" in	
	let lines = addline lines code codepre [] in

	let codepre = "    0|]" in
	let stnumber = let num = getfirstintmax (getlinebefore lines codepre) in string_of_int (num+1) in
	let code = [String.concat "" ["  ";stnumber; " (* "; (String.uppercase funcname); " *);"]] in
	let lines = addlinebefore lines code codepre [] in

	let codepre = "(* Entry tasks *)" in
	let (stnumber1, stnumber2) = get2int_input_parser lines 0 0 in
	let code = [
"; (fun __caml_parser_env ->";
"    let _3 = (Parsing.peek_val __caml_parser_env 1 : Input_parser_env.raw_expr) in";
"    Obj.repr(";
(String.concat "" ["# "; stnumber1; " \"input_parser.mly\""]);
(String.concat "" ["                             ( Raw_u_op (\""; funcname; "\", _3) )"]);
(String.concat "" ["# "; stnumber2; " \"input_parser.ml\""]);
"               : Input_parser_env.raw_expr))"]
in
	let lines = addlinebefore lines code codepre [] in

	let chan = open_out output in
	List.iter (fprintf chan "%s\n") lines;
	close_out chan;;
	

let () = let lines = read_file Sys.argv.(1) in
	 let mode = List.hd lines in
	 let funcname = List.nth lines 1 in
	 if (String.compare mode "Manual" == 0) then
	 let rec getcode strl s c = if c< List.length strl - 4 then getcode strl (s@[List.nth strl c]) (c+1) else s in
	 let l = List.length lines in
	 let code =getcode lines [""] 3 in
	 let floatexpr = List.nth lines (l-3) in
 	 let numexpr = List.nth lines (l-2) in
	 let intervalexpr = List.nth lines (l-1) in

	 write_taylor_form_ml_code funcname code "taylor_form.ml";
	 write_expr_ml funcname "expr.ml";
	 write_expr_mli funcname "expr.mli";
	 write_eval_ml funcname floatexpr numexpr intervalexpr  "eval.ml";
	 write_rounding_simpl_ml funcname intervalexpr "rounding_simpl.ml";
	 write_exprOut_ml funcname intervalexpr "exprOut.ml";
         write_input_lexer_ml funcname "input_lexer.ml";
         write_input_lexer_mll funcname "input_lexer.mll";
	 write_input_parser_ml funcname "input_parser.ml";
	 write_input_parser_mly funcname "input_parser.mly";
	 write_input_parser_env_ml funcname "input_parser_env.ml";

	
	 else if (String.compare mode "Derivative" == 0) then
	  let first_derivative = List.nth lines 2 in
	  let second_derivative = List.nth lines 3 in
	  let floatexpr = List.nth lines 4 in
 	  let numexpr = List.nth lines 5 in
	  let intervalexpr = List.nth lines 6 in

	 write_taylor_form_ml_dev funcname first_derivative second_derivative "taylor_form.ml";
	 write_expr_ml funcname "expr.ml";
	 write_expr_mli funcname "expr.mli";
	 write_eval_ml funcname floatexpr numexpr intervalexpr "eval.ml";
	 write_rounding_simpl_ml funcname intervalexpr "rounding_simpl.ml";
	 write_exprOut_ml funcname intervalexpr "exprOut.ml";
         write_input_lexer_ml funcname "input_lexer.ml";
         write_input_lexer_mll funcname "input_lexer.mll";
	 write_input_parser_ml funcname "input_parser.ml";
	 write_input_parser_mly funcname "input_parser.mly";
	 write_input_parser_env_ml funcname "input_parser_env.ml";


	else if (String.compare mode "Formula" == 0)  then
	  let formula = List.nth lines 2 in

	 write_expr_ml funcname "expr.ml";
	 write_expr_mli funcname "expr.mli";
	 write_exprOut_ml funcname "(%a)" "exprOut.ml";
         write_input_lexer_ml funcname "input_lexer.ml";
         write_input_lexer_mll funcname "input_lexer.mll";
	 write_input_parser_ml funcname "input_parser.ml";
	 write_input_parser_mly funcname "input_parser.mly";
	 write_input_parser_env_ml_code funcname formula "input_parser_env.ml";


	else print_endline "No mode"; let i = Sys.command "make clean" in let j = Sys.command "make all" in print_endline (string_of_int j)
;;
