type result = {
  name : string;
  real_bounds : Interval.interval;
  abs_error_model : Expr.expr option;
  rel_error_model : Expr.expr option;
  (* Lower bounds of error intervals represent lower bounds
     returned by a global optimization procedure.
     low = neg_infinity if a lower bound is not returned. *)
  abs_error_approx : Interval.interval option;
  abs_error_exact : Interval.interval option;
  rel_error_approx : Interval.interval option;
  rel_error_exact : Interval.interval option;
  ulp_error_approx : Interval.interval option;
  ulp_error_exact : Interval.interval option;
  elapsed_time : float;
}

val fptaylor : input_files:string list -> result list