open Core

type letter_hint =
  | Incorrect of char
  | Correct of char
  | In_word of char
[@@deriving sexp, compare]

let explode s = List.init (String.length s) ~f:(String.get s)

let word_diff w1 w2 =
  let w1, w2 = explode w1, explode w2 in
  let rec calc_diff w1 w2 =
    match w1, w2 with
    | c1 :: t1, c2 :: t2 when Char.( = ) c1 c2 -> calc_diff t1 t2
    | _ :: t1, _ :: t2 -> 1 + calc_diff t1 t2
    | _, [] | [], _ -> 0
  in
  calc_diff w1 w2
;;

module type GameRules = sig
  type t [@@deriving sexp, compare]

  val get_start_end : t -> string * string
  val get_hints : t -> target:string -> word:string -> letter_hint list
  val get_new_target : t -> locked_in:letter_hint list -> old_target:string -> string
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
  val rule_data : t -> rule_t
end

module Make (R : GameRules) = struct
  type rule_t = R.t [@@deriving sexp, compare]

  type t =
    { start : string
    ; guesses : letter_hint list list
    ; target : string
    ; rule_data : rule_t
    }
  [@@deriving sexp, compare]

  let new_game rule_t =
    let start, target = R.get_start_end rule_t in
    { start; guesses = []; target; rule_data = rule_t }
  ;;

  let start t = t.start
  let target t = t.target

  let rec letter_hints_to_s hints =
    match hints with
    | Incorrect c :: tl -> Char.to_string c ^ letter_hints_to_s tl
    | Correct c :: tl -> Char.to_string c ^ letter_hints_to_s tl
    | In_word c :: tl -> Char.to_string c ^ letter_hints_to_s tl
    | [] -> ""
  ;;

  let locked_in_letters t =
    match t.guesses with
    | w :: _ ->
      List.map2_exn
        ~f:(fun a b ->
          match a, b with
          | Correct _, w -> Correct w
          | Incorrect _, w -> Incorrect w
          | In_word _, w -> In_word w)
        w
        (explode t.target)
    | [] -> List.map ~f:(fun c -> Incorrect c) (explode t.target)
  ;;

  let game_over g =
    List.fold
      ~f:(fun a x ->
        a
        &&
        match x with
        | Correct _ -> true
        | _ -> false)
      ~init:true
      (locked_in_letters g)
  ;;

  let get_prev g =
    match g.guesses with
    | w :: _ -> letter_hints_to_s w
    | [] -> g.start
  ;;

  let validate_word t w =
    let w = String.lowercase w in
    let diff = word_diff w (get_prev t) in
    (not (game_over t))
    && (diff = 1 || (String.( = ) w t.target && diff = 0))
    && Array.find ~f:(fun x -> String.( = ) x w) Words.all_words |> Option.is_some
  ;;

  let get_locked w1 w2 =
    let rec help w1 w2 =
      match w1, w2 with
      | c1 :: t1, c2 :: t2 when Char.( = ) c1 c2 -> Correct c1 :: help t1 t2
      | c1 :: t1, _ :: t2 -> Incorrect c1 :: help t1 t2
      | [], _ | _, [] -> []
    in
    help (explode w1) (explode w2)
  ;;

  let enter_word t w =
    let target =
      R.get_new_target t.rule_data ~locked_in:(get_locked w t.target) ~old_target:t.target
    in
    if validate_word t w
    then
      if String.( = ) w t.target
      then
        { t with guesses = R.get_hints t.rule_data ~target:t.target ~word:w :: t.guesses }
      else
        { t with
          start = t.start
        ; guesses = R.get_hints t.rule_data ~target:t.target ~word:w :: t.guesses
        ; target
        }
    else failwith "Invalid word"
  ;;

  let game_over g =
    List.fold
      ~f:(fun a x ->
        a
        &&
        match x with
        | Correct _ -> true
        | _ -> false)
      ~init:true
      (locked_in_letters g)
  ;;

  let guesses t = t.guesses
  let rule_data t = t.rule_data
end
