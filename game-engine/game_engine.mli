
type letter_hint =
| Incorrect of char
| Correct of char
| In_word of char
[@@deriving sexp, compare]

val word_diff : string -> string -> int

val explode : string -> char list

module type GameRules = sig
  type t [@@deriving sexp, compare]

  val get_start_end: t -> string*string

  val get_hints: t -> target:string -> word:string -> letter_hint list

  val get_new_target: t -> locked_in: letter_hint list -> old_target: string -> string
end

module type GameEngine = sig
  type t [@@deriving sexp, compare]

  type rule_t [@@deriving sexp, compare]

  val new_game : rule_t -> t
  val start : t -> string
  val target : t -> string
  val validate_word : t -> string -> bool
  val enter_word : t -> string -> t
  val locked_in_letters : t -> letter_hint list
  val game_over : t -> bool
  val guesses : t -> letter_hint list list
end


module Make (R : GameRules) : GameEngine with type rule_t = R.t