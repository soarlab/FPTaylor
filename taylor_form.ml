(* FPTaylor                                                                   *)
(* Alexey Solovyev, University of Utah                                        *)

open Expr

type taylor_form = {
  v0 : expr;
  v1 : expr list;
  m2 : float;
}
