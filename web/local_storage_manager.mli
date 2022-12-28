open! Core

module LocalStorage : sig
  val load_model : string -> deserializer:(Sexp.t -> 'a) -> 'a option
  val save_model : string -> model:'a -> serializer:('a -> Sexp.t) -> unit
end
