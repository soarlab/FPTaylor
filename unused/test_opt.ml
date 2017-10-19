(* Use INTERVAL/ocamlfpu *)

#directory "./INTERVAL";;
#load "INTERVAL/interval.cma";;

open List;;
open Interval;;

let mk_const_interval f = {low = f; high = f};;

let intersect x1 x2 = { 
  low = max x1.low x2.low;
  high = min x1.high x2.high;
};;

let iabs x =
  max (abs_float x.low) (abs_float x.high);;

let mk_list n f =
  let rec mk n acc =
    if n < 0 then acc else mk (n - 1) (f n :: acc) in
  mk (n - 1) [];;

type domain = {
  dim : int;
  bounds : interval array;
  midpoint : float array;
  width : float array;
};;

let dom_of_list bnds = 
  let ms = map (fun b -> (b.low *. 0.5 +. b.high *. 0.5)) bnds in
  let ws = map2 
    (fun b m -> max (Fpu.fsub_high b.high m) (Fpu.fsub_high m b.low))
    bnds ms in 
  {
    dim = length bnds;
    bounds = Array.of_list bnds;
    midpoint = Array.of_list ms;
    width = Array.of_list ws;
  };;

let mk_dom bnds =
  let n = Array.length bnds in
  let ms = Array.map (fun b -> (b.low *. 0.5 +. b.high *. 0.5)) bnds in
  let ws = Array.init n 
    (fun i ->
      let b = bnds.(i) in
      let m = ms.(i) in
      max (Fpu.fsub_high b.high m) (Fpu.fsub_high m b.low)) in
  {
    dim = n;
    bounds = bnds;
    midpoint = ms;
    width = ws;
  };;

let split_dom dom =
  let w = Array.mapi (fun i b -> (i, b.high -. b.low)) dom.bounds in
  let (i, _) = Array.fold_left 
    (fun (i, v1) (j, v2) -> if v1 > v2 then (i, v1) else (j, v2)) (0, neg_infinity) w in
  let bi = dom.bounds.(i) in
  let m = dom.midpoint.(i) in
  let m1 = bi.low *. 0.5 +. m *. 0.5 and
      m2 = m *. 0.5 +. bi.high *. 0.5 in
  let w1 = max (Fpu.fsub_high m1 bi.low) (Fpu.fsub_high m m1) and
      w2 = max (Fpu.fsub_high m2 m) (Fpu.fsub_high bi.high m2) in
  let d1 = {
    dim = dom.dim;
    bounds = (let c = Array.copy dom.bounds in 
	      let _ = c.(i) <- {low = c.(i).low; high = m} in c);
    midpoint = (let c = Array.copy dom.midpoint in
		let _ = c.(i) <- m1 in c);
    width = (let c = Array.copy dom.width in
	     let _ = c.(i) <- w1 in c);
  } in
  let d2 = {
    dim = dom.dim;
    bounds = (let c = Array.copy dom.bounds in 
	      let _ = c.(i) <- {low = m; high = c.(i).high} in c);
    midpoint = (let c = Array.copy dom.midpoint in
		let _ = c.(i) <- m2 in c);
    width = (let c = Array.copy dom.width in
	     let _ = c.(i) <- w2 in c);
  } in
  d1, d2;;

    
type tform1 = {
  f : interval;
  f0 : interval;
  df : interval list;
};;

let tform1_interval dom tf =
  let ( +^ ) = Fpu.fadd_high and
      ( *^ ) = Fpu.fmul_high and
      ( -- ) = Fpu.fsub_low in
  let ws = Array.to_list dom.width in
  let rad = fold_left2 
    (fun r d w -> (iabs d *^ w) +^ r) 0.0 tf.df ws in
  let x1 =  {
    low = tf.f0.low -- rad;
    high = tf.f0.high +^ rad;
  } in
  intersect x1 tf.f;

type expr =
  | Const of interval
  | Var of int
  | Neg of expr
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr;;

let mk_const f = Const {low = f; high = f} and
    mk_var i = Var i and
    mk_neg e = Neg e and
    mk_add e1 e2 = Add (e1, e2) and
    mk_sub e1 e2 = Sub (e1, e2) and
    mk_mul e1 e2 = Mul (e1, e2) and
    mk_div e1 e2 = Div (e1, e2);;

let eval0 =
  let rec eval vars = function
    | Const v -> v
    | Var i -> vars.(i)
    | Neg e ->
      ~-$(eval vars e)
    | Add (e1, e2) ->
      eval vars e1 +$ eval vars e2
    | Sub (e1, e2) ->
      eval vars e1 -$ eval vars e2
    | Mul (e1, e2) ->
      eval vars e1 *$ eval vars e2
    | Div (e1, e2) ->
      eval vars e1 /$ eval vars e2 in
  fun e vars ->
    eval vars e;;

let eval1 =
  let rec eval dom = function
    | Const v -> 
      {
	f = v;
	f0 = v;
	df = mk_list dom.dim (fun i -> zero_I);
      }
    | Var i -> 
      {
	f = dom.bounds.(i);
	f0 = mk_const_interval dom.midpoint.(i);
	df = mk_list dom.dim (fun j -> if i = j then one_I else zero_I);
      }
    | Neg e1 ->
      let r1 = eval dom e1 in
      {
	f = ~-$(r1.f);
	f0 = ~-$(r1.f0);
	df = map (~-$) r1.df;
      }
    | Add (e1, e2) -> 
      let r1 = eval dom e1 and
	  r2 = eval dom e2 in
      {
	f = r1.f +$ r2.f;
	f0 = r1.f0 +$ r2.f0;
	df = map2 (+$) r1.df r2.df;
      }
    | Sub (e1, e2) ->
      let r1 = eval dom e1 and
	  r2 = eval dom e2 in
      {
	f = r1.f -$ r2.f;
	f0 = r1.f0 -$ r2.f0;
	df = map2 (-$) r1.df r2.df;
      }
    | Mul (e1, e2) ->
      let r1 = eval dom e1 and
	  r2 = eval dom e2 in
      let x1 = tform1_interval dom r1 and
	  x2 = tform1_interval dom r2 in
      {
	f = r1.f *$ r2.f;
	f0 = r1.f0 *$ r2.f0;
	df = map2 (fun d1 d2 -> (x1 *$ d2) +$ (x2 *$ d1)) r1.df r2.df;
      }
    | Div (e1, e2) ->
      let r1 = eval dom e1 and
	  r2 = eval dom e2 in
      let x1 = tform1_interval dom r1 and
	  x2 = tform1_interval dom r2 in
      {
	f = r1.f /$ r2.f;
	f0 = r1.f0 /$ r2.f0;
	df = map2 (fun d1 d2 -> ((x2 *$ d1) -$ (x1 *$ d2)) /$ (pow_I_i x2 2)) r1.df r2.df;
      }
  in
  fun e dom ->
    eval dom e;;


(* -------------------------- *)

let counter = ref 0;;

let rec opt0 f x_tol f_tol m bound doms acc =
  match doms with
    | [] -> 
      if acc = [] then
	m, bound
      else
	opt0 f x_tol f_tol m bound acc []
    | dom :: rest ->
      let _ = counter := !counter + 1 in
      let v = eval0 f dom.bounds in
      if v.high <= bound then
	opt0 f x_tol f_tol m bound rest acc
      else
	let d_min = Array.map (fun d -> mk_const_interval d.low) dom.bounds and
	    d_max = Array.map (fun d -> mk_const_interval d.high) dom.bounds and
	    d_mid = Array.map mk_const_interval dom.midpoint in
	let v2_min = eval0 f d_min and
	    v2_max = eval0 f d_max and
	    v2_mid = eval0 f d_mid in
	let v2 = max (max v2_min.low v2_max.low) v2_mid.low in
	let bound = max v2 bound in
	if abs_float (v.high -. v2) <= f_tol || size_max_X dom.bounds <= x_tol then
	  opt0 f x_tol f_tol (max m v.high) bound rest acc
	else
	  let d1, d2 = split_dom dom in
	  opt0 f x_tol f_tol (max m bound) bound rest (d1 :: d2 :: acc);;

let test_opt0 f a x_tol f_tol =
  let _ = counter := 0 in
  let m, bound = opt0 f x_tol f_tol neg_infinity neg_infinity [mk_dom a] [] in
  (m, bound, !counter);;

let rec opt1 f x_tol f_tol m bound doms acc =
  match doms with
    | [] -> 
      if acc = [] then
	m, bound
      else
	opt1 f x_tol f_tol m bound acc []
    | dom :: rest ->
      let _ = counter := !counter + 1 in
      let tf = eval1 f dom in
      let v = tform1_interval dom tf in
      if v.high <= bound then
	opt1 f x_tol f_tol m bound rest acc
      else
	let d_min = Array.map (fun d -> mk_const_interval d.low) dom.bounds and
	    d_max = Array.map (fun d -> mk_const_interval d.high) dom.bounds in
	let v2_min = eval0 f d_min and
	    v2_max = eval0 f d_max and
	    v2_mid = tf.f0 in
	let v2 = max (max v2_min.low v2_max.low) v2_mid.low in
	let bound = max v2 bound in
	if abs_float (v.high -. v2) <= f_tol || size_max_X dom.bounds <= x_tol then
	  opt1 f x_tol f_tol (max m v.high) bound rest acc
	else
	  let d1, d2 = split_dom dom in
	  opt1 f x_tol f_tol (max m bound) bound rest (d1 :: d2 :: acc);;

let test_opt1 f a x_tol f_tol =
  let _ = counter := 0 in
  let m, bound = opt1 f x_tol f_tol neg_infinity neg_infinity [mk_dom a] [] in
  (m, bound, !counter);;

(* --------------------------- *)

let mono_test dom tf =
  let flag = ref false in
  let signs = map
    (fun d -> 
      if d.high <= 0.0 then 
	let _ = flag := true in -1
      else if d.low >= 0.0 then 
	let _ = flag := true in 1
      else 0) tf.df in
  if not !flag then
    false, dom
  else
    let s = Array.of_list signs in
    let n = dom.dim in
    true, {
      dim = n;
      bounds = Array.init n
	(fun i ->
	  let b = dom.bounds.(i) in
	  if s.(i) > 0 then mk_const_interval b.high
	  else if s.(i) < 0 then mk_const_interval b.low
	  else b);
      midpoint = Array.init n
	(fun i ->
	  let m = dom.midpoint.(i) in
	  let b = dom.bounds.(i) in
	  if s.(i) > 0 then b.high
	  else if s.(i) < 0 then b.low
	  else m);
      width = Array.init n
	(fun i ->
	  let w = dom.width.(i) in
	  if s.(i) <> 0 then 0.0 else w);
    };;

let rec opt3 f x_tol f_tol m bound doms acc =
  match doms with
    | [] -> 
      if acc = [] then
	m, bound
      else
	let doms0 = sort (fun (v1, _) (v2, _) -> compare v2 v1) acc in
	let doms1 = map snd doms0 in
	opt3 f x_tol f_tol m bound doms1 []
    | dom :: rest ->
      let _ = counter := !counter + 1 in
      let tf = eval1 f dom in
      
      let mono_flag, dom' = mono_test dom tf in
      let dom, tf = 
	if mono_flag then
	  dom', eval1 f dom'
	else
	  dom, tf in

      let v = tform1_interval dom tf in
      if v.high <= bound then
	opt3 f x_tol f_tol m bound rest acc
      else
	let d_min = Array.map (fun d -> mk_const_interval d.low) dom.bounds and
	    d_max = Array.map (fun d -> mk_const_interval d.high) dom.bounds in
	let v2_min = eval0 f d_min and
	    v2_max = eval0 f d_max and
	    v2_mid = tf.f0 in
	let v2 = max (max v2_min.low v2_max.low) v2_mid.low in
	let bound = max v2 bound in
	if abs_float (v.high -. v2) <= f_tol || size_max_X dom.bounds <= x_tol then
	  opt3 f x_tol f_tol (max m v.high) bound rest acc
	else
	  let d1, d2 = split_dom dom in
	  opt3 f x_tol f_tol (max m bound) bound rest ((v2, d1) :: (v2, d2) :: acc);;

let test_opt3 f a x_tol f_tol =
  let _ = counter := 0 in
  let m, bound = opt3 f x_tol f_tol neg_infinity neg_infinity [mk_dom a] [] in
  (m, bound, !counter);;


(* -------------------------------- *)

let report_flag = ref false;;
let report str = 
  if !report_flag then print_endline str else ();;

type cond_result = Sat | Unsat | Unknown;;

let eval0_cond c dom =
  let f = eval0 c dom.bounds in
  if f.high <= 0.0 then Sat
  else if f.low > 0.0 then Unsat
  else Unknown;;

(*
let eval1_cond c dom =
  let tf = eval1 c dom.bounds in
  let x = tform1_interval dom tf in
  if f.high <= 0.0 then Sat
  else if f.low > 0.0 then Unsat
  else Unknown;;
*)

let rec eval_conds cs dom =
  match cs with
    | [] -> Sat
    | c :: rest ->
      begin
	match eval0_cond c dom with
	  | Sat -> eval_conds rest dom
	  | Unsat -> Unsat
	  | Unknown ->
	    if eval_conds rest dom = Unsat then Unsat else Unknown
      end;;

let eval_cond_pt c x =
  let f = eval0 c x in
  if f.high <= 0.0 then Sat
  else if f.low > 0.0 then Unsat
  else Unknown;;

let rec eval_conds_pt cs x =
  match cs with
    | [] -> Sat
    | c :: rest ->
      begin
	match eval_cond_pt c x with
	  | Sat -> eval_conds_pt rest x
	  | Unsat -> Unsat
	  | Unknown ->
	    if eval_conds_pt rest x = Unsat then Unsat else Unknown
      end;;

let reset_stat, update_stat, print_stat =
  let sat = ref 0 and
      unsat = ref 0 and
      unknown = ref 0 in
  let reset () =
    sat := 0;
    unsat := 0;
    unknown := 0 in
  let update r =
    match r with
      | Sat -> incr sat
      | Unsat -> incr unsat
      | Unknown -> incr unknown in
  let print () =
    let str = Printf.sprintf "sat = %d, unsat = %d, unknown = %d" !sat !unsat !unknown in
    print_endline str in
  reset, update, print;;

let rec estimate_mono dom f =
  if size_max_X dom.bounds <= 1e-5 then
    eval1 f dom, dom
  else
    let tf = eval1 f dom in
    let mono_flag, dom' = mono_test dom tf in
    if mono_flag then
      estimate_mono dom' f
    else
      tf, dom';;

let opt4 f conds x_tol f_tol max_iter m bound doms =
  let rec opt m bound doms acc =
    match doms with
      | [] -> 
	if acc = [] then
	  m, bound
	else
	  let doms0 = sort (fun (v1, _) (v2, _) -> compare v2 v1) acc in
	  let doms1 = map snd doms0 in
	  opt m bound doms1 []
      | dom :: rest ->
	begin
	  let _ = counter := !counter + 1 in

	  let r = eval_conds conds dom in
	  let _ = update_stat r in
	  match r with
	    | Unsat ->
	      let _ = report "Unsat" in
	      opt m bound rest acc
	    | Unknown ->
	      let _ = report "Unknown" in

(*
	    let tf, dom' = estimate_mono dom f in
	    let v = tform1_interval dom' tf in
*)
	      let tf = eval1 f dom in

	      let mono_flag, dom' = mono_test dom tf in
	      let tf = if mono_flag then eval1 f dom' else tf in
	      let v = tform1_interval dom' tf in

(*	    let v = tform1_interval dom tf in *)
	      if v.high <= bound +. f_tol || !counter >= max_iter then
		opt (max m v.high) bound rest acc
	      else
		let eval_cs x =
		  if eval_conds_pt conds x = Sat then 
		    (eval0 f x).low
		  else 
		    neg_infinity in
		let d_min = Array.map (fun d -> mk_const_interval d.low) dom.bounds and
		    d_max = Array.map (fun d -> mk_const_interval d.high) dom.bounds and
		    d_mid = Array.map (fun d -> mk_const_interval d) dom.midpoint in
		let v2_min = eval_cs d_min and
		    v2_max = eval_cs d_max and
		    v2_mid = eval_cs d_mid in
		let v2 = max (max v2_min v2_max) v2_mid in
		let bound = max v2 bound in
		if abs_float (v.high -. v2) <= f_tol || size_max_X dom.bounds <= x_tol then
		  opt (max m v.high) bound rest acc
		else
		  let d1, d2 = split_dom dom in
		  let x = if v2 > neg_infinity then v2 else v.high in
		  opt m bound rest ((x, d1) :: (x, d2) :: acc)
	    | Sat ->
	      let _ = report "Sat" in
	      let tf = eval1 f dom in

	      let mono_flag, dom' = mono_test dom tf in
	      let dom, tf = 
		if mono_flag then
		  dom', eval1 f dom'
		else
		  dom, tf in

	      let v = tform1_interval dom tf in
	      if v.high <= bound +. f_tol || !counter >= max_iter then
		opt (max m v.high) bound rest acc
	      else
		let d_min = Array.map (fun d -> mk_const_interval d.low) dom.bounds and
		    d_max = Array.map (fun d -> mk_const_interval d.high) dom.bounds in
		let v2_min = eval0 f d_min and
		    v2_max = eval0 f d_max and
		    v2_mid = tf.f0 in
		let v2 = max (max v2_min.low v2_max.low) v2_mid.low in
		let bound = max v2 bound in
		if abs_float (v.high -. v2) <= f_tol || size_max_X dom.bounds <= x_tol then
		  opt (max m v.high) bound rest acc
		else
		  let d1, d2 = split_dom dom in
		  opt m bound rest ((v2, d1) :: (v2, d2) :: acc)
	end in
  opt m bound doms [];;

(* All conditions are assumed to be (g x <= 0) *)
let test_opt4 f conds a x_tol f_tol max_iter =
  let _ = reset_stat() in
  let _ = counter := 0 in
  let m, bound = opt4 f conds x_tol f_tol max_iter neg_infinity neg_infinity [mk_dom a] in
  (m, bound, !counter);;


(* ------------------------ *)

test_opt0 e1 x0 0.0 0.01;;
test_opt1 e1 x0 0.0 0.01;;
test_opt3 e1 x0 0.0 0.01;;
test_opt4 e1 [] x0 0.0 0.01 10000;;
test_opt0 e2 x0 0.0 0.01;;
test_opt1 e2 x0 0.0 0.01;;
test_opt3 e2 x0 0.0 0.01;;
test_opt4 e2 [] x0 0.0 0.01 10000;;

test_opt0 (mk_neg e2) x0 0.0 0.01;;
test_opt1 (mk_neg e2) x0 0.0 0.01;;
test_opt3 (mk_neg e2) x0 0.0 0.01;;
test_opt4 (mk_neg e2) [] x0 0.0 0.01 10000;;

test_opt4 e0 [c1; c2; c3] x0 0.0001 0.01;;
test_opt4 e1 [c1; c2; c3] x0 0.0001 0.01;;
test_opt4 e2 [c1; c2; c3] x0 0.0001 0.01;;
test_opt4 (mk_neg e2) [c1; c2; c3] x0 0.01 0.01 1000000;;

let m, bound, c =
  let x_tol = 0.01 and
      f_tol = 0.01 and
      max_iter = 1000000 in
  let bound0 = 0.0 in
  let _ = reset_stat() in
  let _ = counter := 0 in
  let m, bound = opt4 (mk_neg e2) [c1; c2; c3] x_tol f_tol max_iter 
    neg_infinity bound0 [mk_dom x0] in
  (m, bound, !counter);;

print_stat();;

let e0, e1, e2, c1, c2, c3 =
  let ( + ) = mk_add and
      ( - ) = mk_sub and
      ( * ) = mk_mul and
      ( / ) = mk_div in
  let two = mk_const 2.0 in
  let eps = mk_const 0.1 in
  let a = mk_var 0 and
      b = mk_var 1 and
      c = mk_var 2 in
  let s = (a + b + c) / two in
  s,
  s * (s - a),
  s * (s - a) * (s - b) * (s - c),
  eps + c - a - b,
  eps + a - b - c,
  eps + b - c - a;;

let x0 = Array.init 3 (fun i -> {low = 1.0; high = 9.0});;


let e0, e1, c1, c2, c3 =
  let ( + ) = mk_add and
      ( - ) = mk_sub and
      ( * ) = mk_mul in
  let four = mk_const 4.0 in
  let eps = mk_const 0.1 in
  let a = mk_var 0 and
      b = mk_var 1 and
      c = mk_var 2 in
  b * b - a * c * four,
  b * b - a * c,
  eps + a * c * four - b * b,
  mk_const 10.0 + a * c - b * b,
  mk_neg b;;

let x0 = Array.init 3 
  (function
    | 0 -> {low = 1.0; high = 3.0}
    | 1 -> {low = 1.5; high = 3.5}
    | 2 -> {low = -2.0; high = 2.0}
    | _ -> failwith "init");;

test_opt4 e0 [] x0 0.01 0.01;;
test_opt4 e1 [] x0 0.01 0.01;;

test_opt4 (mk_neg e0) [] x0 0.01 0.01;;
test_opt4 (mk_neg e1) [] x0 0.01 0.01;;

test_opt4 e0 [c1; c2; c3] x0 0.01 0.01;;
test_opt4 e1 [c1; c2; c3] x0 0.01 0.01;;

test_opt4 (mk_neg e0) [c1; c3] x0 0.01 0.01;;
test_opt4 (mk_neg e1) [c1; c2; c3] x0 0.1 0.01;;
print_stat();;

let dom0 = mk_dom x0;;


eval0 e0 x0;;
eval0 e1 x0;;
eval0 e2 x0;;

let f0 = eval1 e0 dom0;;
tform1_interval dom0 f0;;

let f1 = eval1 e1 dom0;;
tform1_interval dom0 f1;;

let f2 = eval1 e2 dom0;;
tform1_interval dom0 f2;;

let dom1, dom2 = split_dom dom0;;
eval1 e0 dom1;;

let tf1 = eval1 e1 dom1;;
mono_test dom1 tf1;;

let tf2 = eval1 e1 dom2;;
mono_test dom2 tf2;;



tform1_interval dom2 (eval1 e1 dom2);;
eval0 e1 dom2.bounds;;


(*************************)

#use "INTERVAL/EXAMPLES/B_AND_B/pqueue.ml";;

sort;;

let opt2 f x0 x_tol f_tol =
  let m = ref neg_infinity and
      bound = ref neg_infinity and
      q = ref (empty()) and
      iter = ref 0 in
  let (_, newq) = insert neg_infinity (mk_dom x0) !q in
  let _ = q := newq in
  try
    while true do
      let _ = incr iter in
      let (_, dom, newq) = extract !q in
      let _ = q := newq in
      let tf = eval1 f dom in
      let v = tform1_interval dom tf in
      if v.high > !bound then
	let d_min = Array.map (fun d -> mk_const_interval d.low) dom.bounds and
	    d_max = Array.map (fun d -> mk_const_interval d.high) dom.bounds in
	let v2_min = eval0 f d_min and
	    v2_max = eval0 f d_max and
	    v2_mid = tf.f0 in
	let v2 = max (max v2_min.low v2_max.low) v2_mid.low in
	let _ = bound := max v2 !bound in
	if abs_float (v.high -. v2) <= f_tol || size_max_X dom.bounds <= x_tol then
	  m := max v.high !m
	else
	  let d1, d2 = split_dom dom in
(*	  let x1 = (eval0 f d1.bounds).high and
	      x2 = (eval0 f d2.bounds).high in*)
	  let (_, newq) = insert v2 d1 !q in
	  let (_, newq) = insert v2 d2 newq in
	  q := newq;
	  m := max !bound !m
    done;
    failwith "Should never happen"
  with Not_found -> (!m, !bound, !iter);;

opt2 e0 x0 0.0 0.01;;
opt2 e1 x0 0.0 0.01;;
opt2 e2 x0 0.0 0.01;;
opt2 (mk_neg e2) x0 0.0 0.01;;

