module GameEngine: sig
  type t [@@deriving sexp, compare]

  type game_mode = 
      Shuffle
    | Normal of int
  [@@deriving sexp, compare]

  val todays_game: unit -> game_mode

  val new_game: game_mode -> t

  val is_normal: t -> bool

  val start: t -> string

  val target: t -> string

  val mode: t -> game_mode

  val validate_word: t -> string -> bool

  val enter_word: t -> string -> t

  val locked_in_letters: t -> char option list

  val game_over: t -> bool

  val guesses: t -> string list
end