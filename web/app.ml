open Core
open Incr_dom
module SolutionMap = Model.SolutionMap
module NormalGame = Game.Make (Game.NormalRule)
module ShuffleGame = Game.Make (Game.ShuffleRule)
module Model = Model

module Action = struct
  type t =
    | Enter_word
    | Enter_char of char
    | Backspace
    | Reset_game
    | Switch_mode
    | Show_hint
    | Close_stats
  [@@deriving sexp]
end

module State = struct
  type t = unit
end

let apply_action model action _ ~schedule_action:_ =
  match (action : Action.t) with
  | Enter_word -> Model.enter_word model
  | Enter_char c -> Model.update_input model (model.input ^ Char.to_string c)
  | Backspace ->
    let input = Model.input model in
    if String.( = ) input ""
    then model
    else if String.length input = 1
    then Model.update_input model ""
    else
      Model.update_input
        model
        (String.slice (Model.input model) 0 (String.length (Model.input model) - 1))
  | Reset_game -> Model.reset_game model
  | Switch_mode -> Model.switch_gamemode model
  | Show_hint -> Model.set_show_hint model
  | Close_stats -> Model.close_stats model
;;

let on_startup ~schedule_action:_ _ = Async_kernel.return ()

let handle_keyup keycode ~inject =
  if keycode >= 65 && keycode <= 90
  then (
    match Char.of_int keycode with
    | Some c -> inject (Action.Enter_char c)
    | None -> failwith "impossible")
  else if keycode = 8
  then inject Action.Backspace
  else if keycode = 13
  then inject Action.Enter_word
  else Ui_effect.return ()
;;

let build_word ?(attrs = []) w =
  let open Vdom in
  let spans =
    List.map
      ~f:(fun h ->
        let c, color =
          match h with
          | Game.Correct c -> c, "#538d4e"
          | Game.In_word c -> c, "#b59f3b"
          | Game.Incorrect c -> c, "white"
        in
        Node.span
          ~attr:
            (Attr.many_without_merge
               ([ Attr.style (Css_gen.color (`Name color)) ] @ attrs))
          [ Node.text (c |> Char.to_string |> String.uppercase) ])
      w
  in
  Node.div spans
;;

let view (m : Model.t Incr.t) ~inject =
  let open Incr.Let_syntax in
  let open Vdom in
  let keyboard =
    Keyboard.gen_keyboard
      ~on_click:(fun c -> inject (Action.Enter_char c))
      ~on_delete:(fun () -> inject Action.Backspace)
      ~on_enter:(fun () -> inject Action.Enter_word)
  in
  let%map start_label =
    let%map start_txt = m >>| Model.start >>| String.uppercase in
    Node.div
      ~attr:(Attr.many_without_merge [ Attr.style (Css_gen.color (`Name "white")) ])
      [ Node.text start_txt ]
  and guess_list =
    let%map guesses = m >>| Model.guesses in
    List.map ~f:build_word guesses |> List.rev
  and input =
    let%map input_text = m >>| Model.input in
    Node.div
      ~attr:(Attr.many_without_merge [ Attr.style (Css_gen.color (`Name "#b59f3b")) ])
      [ Node.text (input_text ^ "\u{200B}") ]
  and target_label =
    let%map locked = m >>| Model.locked_in_letters
    and show_hint = m >>| Model.show_hint in
    if show_hint then build_word ~attrs:[ Attr.id "target" ] locked else Node.div []
  and shuffle_icon =
    let%map is_normal = m >>| Model.is_normal in
    let image = if is_normal then "shuffle.png" else "loop.png" in
    let style =
      let ( @> ) = Css_gen.( @> ) in
      Css_gen.width (`Em 2)
      @> Css_gen.height (`Em 2)
      @> Css_gen.position ~top:(`Em 2) ~right:(`Em 2) `Fixed
      @> Css_gen.of_string_css_exn "filter: invert(100%)"
    in
    Node.create
      "img"
      ~attr:
        (Attr.many_without_merge
           [ Attr.src image
           ; Attr.style style
           ; Attr.on_click (fun _ -> inject Action.Switch_mode)
           ])
      []
  and hint_button =
    let%map show_hint = m >>| Model.show_hint
    and is_normal = m >>| Model.is_normal in
    if (not show_hint) && is_normal
    then
      Node.button
        ~attr:
          (Attr.many_without_merge
             [ Attr.id "hint-btn"; Attr.on_click (fun _ -> inject Action.Show_hint) ])
        [ Node.text "Show hint" ]
    else Node.div []
  and reset_button =
    let%map is_normal = m >>| Model.is_normal in
    if not is_normal
    then
      Node.button
        ~attr:
          (Attr.many_without_merge
             [ Attr.id "reset"; Attr.on_click (fun _ -> inject Action.Reset_game) ])
        [ Node.text "New Game" ]
    else Node.div []
  and game_stats =
    let gm =
      let%map m = m in
      match m.game with
      | Normal gm -> Some gm
      | Shuffle _ -> None
    in
    let args =
      let%map gm = gm in
      Option.map gm ~f:(fun gm -> gm.game_stats_closed, gm.game, gm.solution_history)
    in
    args
    >>| fun args ->
    Option.map args ~f:(fun (is_closed, game, solution_history) ->
        Game_stats.view ~is_closed ~game ~solution_history ~close_stats:(fun _ ->
            inject Action.Close_stats))
    |> Option.value ~default:(Node.div [])
  and is_over = m >>| Model.game_over in
  let input_target =
    if is_over
    then [ target_label; reset_button ]
    else [ input; target_label; hint_button ]
  in
  Node.body
    ~attr:
      (Attr.many_without_merge
         [ Attr.on_keyup (fun e -> handle_keyup e##.keyCode ~inject) ])
    [ shuffle_icon
    ; Node.div
        ~attr:(Attr.many_without_merge [ Attr.id "game" ])
        ([ start_label ] @ guess_list @ input_target)
    ; game_stats
    ; keyboard
    ]
;;

let on_display ~old_model model _ ~schedule_action:_ =
  if Model.compare old_model model <> 0 then Js_misc.scroll ~id:"keyboard-cont" () else ()
;;

let create model ~old_model ~inject =
  let open Incr.Let_syntax in
  let%map apply_action =
    let%map model = model in
    apply_action model
  and view = view model ~inject
  and model = model
  and on_display =
    let%map old_model = old_model
    and model = model in
    on_display ~old_model model
  in
  Component.create ~apply_action ~on_display model view
;;

let initial_model = Model.get_default ()
