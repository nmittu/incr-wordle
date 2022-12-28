open Core

module GameEngine' = struct
  type game_mode =
    | Shuffle
    | Normal of int
  [@@deriving sexp, compare]

  type letter_hint =
    | Incorrect of char
    | Correct of char
    | In_word of char
  [@@deriving sexp, compare]

  type t =
    { game_mode : game_mode
    ; start : string
    ; guesses : letter_hint list list
    ; target : string
    }
  [@@deriving sexp, fields, compare]
end

module SolutionMap' = Map.Make (Int)

module Model' = struct
  type t =
    { game : GameEngine'.t
    ; input : string
    ; show_hint : bool
    ; solution_history : int SolutionMap'.t [@default SolutionMap'.empty]
    ; game_stats_closed : bool [@default false] [@sexp_drop_if fun _ -> true]
    }
  [@@deriving sexp, fields, compare]
end

let enter_guesses g glist =
  List.fold
    ~f:(fun g w -> Model_intf.NormalGame.enter_word g w)
    ~init:g
    (List.rev_map
       ~f:(fun g ->
         String.concat
           (List.map
              ~f:(fun g ->
                let open GameEngine' in
                match g with
                | Correct c -> Char.to_string c
                | Incorrect c -> Char.to_string c
                | In_word c -> Char.to_string c)
              g))
       glist)
;;

let convert sexp : Model_intf.normal_mode option =
  try
    let model = Model'.t_of_sexp sexp in
    match model.game.game_mode with
    | Normal id ->
      let g = Model_intf.NormalGame.new_game id in
      let g = enter_guesses g (GameEngine'.guesses model.game) in
      Some
        { game = g
        ; show_hint = model.show_hint
        ; solution_history =
            model.solution_history
            |> SolutionMap'.to_alist
            |> Model_intf.SolutionMap.of_alist_exn
        ; game_stats_closed = model.game_stats_closed
        }
    | _ -> failwith "lol 2"
  with
  | e -> raise e
;;
