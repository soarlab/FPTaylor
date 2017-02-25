include Interval1

let zero_I = zero_interval

let one_I = one_interval

let fprintf_I fp format i = 
  Printf.fprintf fp "[%s, %s]" 
    (Printf.sprintf format i.low) (Printf.sprintf format i.high) 

let sprintf_I format i = 
  Printf.sprintf "[%s, %s]" 
    (Printf.sprintf format i.low) (Printf.sprintf format i.high) 

let compare_I_f {low = a; high = b} x =
  if b < x then 1 else if a <= x then 0 else -1
              
let abs_I = abs_i

let max_I_I = max_ii

let min_I_I = min_ii
              
let (~-$) = neg_i

let (+$) = add_ii

let (-$) = sub_ii

let ( *$) = mul_ii

let ( *.$) = mul_di

let ( *$.) = mul_id
              
let (/$) = div_ii

let (/.$) = div_di

let (/$.) = div_id

let inv_I = inv_i
             
let sqrt_I = sqrt_i

let sqr_I = sqr_i

let pow_I_i = pown_i
              
let ( **$.) (x:interval) (f:float) = failwith "(**$.): Not implemented" 
               
let exp_I = exp_i

let log_I = log_i

let cos_I = cos_i

let sin_I = sin_i

let tan_I x = failwith "tan_I: Not implemented"

let asin_I x = failwith "asin_I: Not implemented"

let acos_I x = failwith "acos_I: Not implemented"

let atan_I x = failwith "atan_I: Not implemented"

let sinh_I x = failwith "sinh_I: Not implemented"

let cosh_I x = failwith "cosh_I: Not implemented"

let tanh_I x = failwith "tanh_I: Not implemented"

let size_max_X v =
  Array.fold_left (fun m {low = a; high = b} -> max m (fsub_high b a)) 0.0 v
