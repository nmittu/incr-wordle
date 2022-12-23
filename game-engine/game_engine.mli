module GameEngine: sig
  type t [@@deriving sexp, compare]

  val new_game: unit -> t

  val start: t -> string

  val target: t -> string

  val validate_word: t -> string -> bool

  val enter_word: t -> string -> t

  val locked_in_letters: t -> char option list

  val game_over: t -> bool

  val guesses: t -> string list
end