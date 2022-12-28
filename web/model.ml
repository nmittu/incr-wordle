open! Core
open Local_storage_manager
include Model_intf

let init game = { game; input = "" }

let get_default () =
  let todays_game =
    { game = NormalGame.new_game (Game.NormalRule.get_today ())
    ; show_hint = false
    ; solution_history = SolutionMap.empty
    ; game_stats_closed = false
    }
  in
  let loaded =
    Option.value
      ~default:todays_game
      (LocalStorage.load_model
         "model"
         ~deserializer:(Converters.convert normal_mode_of_sexp))
  in
  if NormalGame.rule_data loaded.game = Game.NormalRule.get_today ()
  then init (Normal loaded)
  else init (Normal { todays_game with solution_history = loaded.solution_history })
;;

let enter_word t =
  let helper
      (type a)
      (m : (module Game.GameEngine with type t = a))
      (g : a)
      ~create_game_mode
    =
    let module M = (val m) in
    let g =
      if M.validate_word g t.input
      then (
        let g = M.enter_word g (String.lowercase t.input) in
        g)
      else g
    in
    { game = create_game_mode g; input = "" }
  in
  match t.game with
  | Normal gm ->
    helper
      (module NormalGame)
      gm.game
      ~create_game_mode:(fun g ->
        let sm =
          if NormalGame.game_over g
          then (
            let score = List.length (NormalGame.guesses g) in
            let opt = SolutionMap.find gm.solution_history score in
            let updated_val = Option.value opt ~default:0 + 1 in
            SolutionMap.set gm.solution_history ~key:score ~data:updated_val)
          else gm.solution_history
        in
        let gm = { gm with game = g; solution_history = sm } in
        LocalStorage.save_model ~model:gm ~serializer:sexp_of_normal_mode "model";
        Normal gm)
  | Shuffle g -> helper (module ShuffleGame) g ~create_game_mode:(fun g -> Shuffle g)
;;

let update_input t w = { t with input = w }

let reset_game m =
  init
    (match m.game with
    | Normal g -> Normal g (* We dont reset normal games! *)
    | Shuffle _ -> Shuffle (ShuffleGame.new_game ()))
;;

let switch_gamemode m =
  match m.game with
  | Normal _ -> init (Shuffle (ShuffleGame.new_game ()))
  | Shuffle _ -> get_default ()
;;

let set_show_hint m =
  match m.game with
  | Normal gm ->
    let gm = { gm with show_hint = true } in
    LocalStorage.save_model ~model:gm ~serializer:sexp_of_normal_mode "model";
    { m with game = Normal gm }
  | Shuffle _ -> m
;;

let close_stats m =
  match m.game with
  | Normal gm -> { m with game = Normal { gm with game_stats_closed = true } }
  | Shuffle _ -> m
;;

let start m =
  match m.game with
  | Normal gm -> NormalGame.start gm.game
  | Shuffle g -> ShuffleGame.start g
;;

let guesses m =
  match m.game with
  | Normal gm -> NormalGame.guesses gm.game
  | Shuffle g -> ShuffleGame.guesses g
;;

let locked_in_letters m =
  match m.game with
  | Normal gm -> NormalGame.locked_in_letters gm.game
  | Shuffle g -> ShuffleGame.locked_in_letters g
;;

let is_normal m =
  match m.game with
  | Normal _ -> true
  | _ -> false
;;

let game_over m =
  match m.game with
  | Normal gm -> NormalGame.game_over gm.game
  | Shuffle g -> ShuffleGame.game_over g
;;

let show_hint m =
  match m.game with
  | Normal gm -> gm.show_hint
  | Shuffle _ -> true
;;

let game_stats_closed m =
  match m.game with
  | Normal gm -> gm.game_stats_closed
  | Shuffle _ -> true
;;

let solution_history m =
  match m.game with
  | Normal gm -> Some gm.solution_history
  | Shuffle _ -> None
;;

let cutoff t1 t2 = compare t1 t2 = 0
