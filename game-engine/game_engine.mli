module GameEngine : sig
  type t [@@deriving sexp, compare]

  type game_mode =
    | Shuffle
    | Normal of int
  [@@deriving sexp, compare]

  type letter_hint =
    | Incorrect of char
    | Correct of char
    | In_word of char
  [@@deriving sexp, compare]

  val todays_game : unit -> game_mode
  val new_game : game_mode -> t
  val is_todays_game : t -> bool
  val is_normal : t -> bool
  val start : t -> string
  val target : t -> string
  val mode : t -> game_mode
  val validate_word : t -> string -> bool
  val enter_word : t -> string -> t
  val locked_in_letters : t -> letter_hint list
  val game_over : t -> bool
  val guesses : t -> letter_hint list list
end
