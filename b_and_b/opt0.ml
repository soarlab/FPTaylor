open Interval

let mk_const_interval v = {low = v; high = v}

type dom = {
  bounds : interval array;
  mid : float array;
}

let mk_dom a = {
  bounds = a;
  mid = Array.map (fun i -> (i.low +. i.high) *. 0.5) a;
}

let split_dom dom =
  let w = Array.mapi (fun i b -> (i, b.high -. b.low)) dom.bounds in
  let (i, _) = Array.fold_left 
    (fun (i, v1) (j, v2) -> if v1 > v2 then (i, v1) else (j, v2)) (0, neg_infinity) w in
  let bi = dom.bounds.(i) in
  let m = dom.mid.(i) in
  let m1 = (bi.low +. m) *. 0.5 and
      m2 = (m +. bi.high) *. 0.5 in
  let d1 = {
    bounds = (let c = Array.copy dom.bounds in 
	      let _ = c.(i) <- {low = c.(i).low; high = m} in c);
    mid = (let c = Array.copy dom.mid in
	   let _ = c.(i) <- m1 in c);
  } in
  let d2 = {
    bounds = (let c = Array.copy dom.bounds in 
	      let _ = c.(i) <- {low = m; high = c.(i).high} in c);
    mid = (let c = Array.copy dom.mid in
	   let _ = c.(i) <- m2 in c);
  } in
  d1, d2


let counter = ref 0

let rec opt0 f x_tol f_tol m bound doms acc =
  let _ = counter := !counter + 1 in
  match doms with
    | [] -> 
      if acc = [] then
	m, bound
      else
	opt0 f x_tol f_tol m bound acc []
    | dom :: rest ->
      let v = f dom.bounds in
      if v.high <= bound then
	opt0 f x_tol f_tol m bound rest acc
      else
	let d_min = Array.map (fun d -> mk_const_interval d.low) dom.bounds and
	    d_max = Array.map (fun d -> mk_const_interval d.high) dom.bounds and
	    d_mid = Array.map mk_const_interval dom.mid in
	let v2_min = f d_min and
	    v2_max = f d_max and
	    v2_mid = f d_mid in
	let v2 = max (max v2_min.low v2_max.low) v2_mid.low in
	let bound = max v2 bound in
	if abs_float (v.high -. v2) <= f_tol || size_max_X dom.bounds <= x_tol then
	  opt0 f x_tol f_tol (max m v.high) bound rest acc
	else
	  let d1, d2 = split_dom dom in
	  opt0 f x_tol f_tol (max m bound) bound rest (d1 :: d2 :: acc)

let opt f a x_tol f_tol =
  let _ = counter := 0 in
  let m, bound = opt0 f x_tol f_tol neg_infinity neg_infinity [mk_dom a] [] in
  (m, bound, !counter)
