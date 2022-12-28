open! Core
open Game_engine

module Rule : sig
  include GameRules with type t = int

  val get_today : unit -> t
end
