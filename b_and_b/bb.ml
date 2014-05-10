open Interval


let start_interval = Array.init 1 (function
| 0 -> {low = 1.001000; high = 2.000000}
| _ -> failwith "Out of boundaries"
)

let f_x input_array = 
  let x = input_array.(0) in
((((((x ** 2.) *. (abs_float(((x ** 2.) -. 1.)))) +. (3. *. (x ** 4.))) -. (6. *. (x ** 2.))) +. 3.) /. ((((x +. 1.) ** 2.) *. (abs_float((x -. 1.)))) *. (abs_float(((x ** 2.) -. 1.)))))
let f_X input_array = 
  let x = input_array.(0) in
((((((pow_I_i x (2)) *$ (abs_I(((pow_I_i x (2)) -$ {low = 1.000000; high = 1.000000})))) +$ ({low = 3.000000; high = 3.000000} *$ (pow_I_i x (4)))) -$ ({low = 6.000000; high = 6.000000} *$ (pow_I_i x (2)))) +$ {low = 3.000000; high = 3.000000}) /$ (((pow_I_i (x +$ {low = 1.000000; high = 1.000000}) (2)) *$ (abs_I((x -$ {low = 1.000000; high = 1.000000})))) *$ (abs_I(((pow_I_i x (2)) -$ {low = 1.000000; high = 1.000000})))))

let _ =
  let int, fint, p, pv = B_and_b.branch_and_bound f_x f_X start_interval 0.010000 0.010000 in
  let _ = print_I fint; print_newline ();
          Printf.printf "max = %f\n" fint.high in
  let int, fint, p, pv = B_and_b.branch_and_bound (fun x -> -. (f_x x)) (fun x -> ~-$ (f_X x)) start_interval 0.010000 0.010000 in
  let _ = print_I fint; print_newline ();
          Printf.printf "min = %f\n" (-. fint.high) in
  flush stdout
