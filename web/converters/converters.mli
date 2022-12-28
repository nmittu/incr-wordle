open! Core

val convert
  :  (Sexp.t -> Model_intf.normal_mode)
  -> Sexp.t
  -> Model_intf.normal_mode option
