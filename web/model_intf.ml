open! Core

module NormalGame = Game.Make(Game.NormalRule)
module ShuffleGame = Game.Make(Game.ShuffleRule)
module SolutionMap = Map.Make(Int)

type normal_mode = {
  game: NormalGame.t;
  show_hint: bool;
  solution_history: int SolutionMap.t [@default SolutionMap.empty]
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
    } [@@deriving sexp, fields, compare]