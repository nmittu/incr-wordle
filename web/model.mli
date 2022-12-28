open! Core
module NormalGame : Game.GameEngine with type rule_t = Game.NormalRule.t
module ShuffleGame : Game.GameEngine with type rule_t = Game.ShuffleRule.t
module SolutionMap : Map.S with type Key.t = int

type normal_mode =
  { game : NormalGame.t
  ; show_hint : bool
  ; solution_history : int SolutionMap.t [@default SolutionMap.empty]
  ; game_stats_closed : bool [@default false] [@sexp_drop_if fun _ -> true]
  }
[@@deriving sexp, compare]

type game =
  | Normal of normal_mode
  | Shuffle of ShuffleGame.t
[@@deriving sexp, compare]

type t =
  { game : game
  ; input : string
  }
[@@deriving sexp, fields, compare]

val init : game -> t
val get_default : unit -> t
val enter_word : t -> t
val update_input : t -> string -> t
val reset_game : t -> t
val switch_gamemode : t -> t
val set_show_hint : t -> t
val close_stats : t -> t
val start : t -> string
val guesses : t -> Game.letter_hint list list
val locked_in_letters : t -> Game.letter_hint list
val is_normal : t -> bool
val game_over : t -> bool
val show_hint : t -> bool
val game_stats_closed : t -> bool
val solution_history : t -> int SolutionMap.t option
val cutoff : t -> t -> bool
