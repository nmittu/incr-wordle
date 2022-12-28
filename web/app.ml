open Core
open Incr_dom
open Game_engine
open Local_storage_manager

module Model = struct
  type t =
    { game : GameEngine.t
    ; input : string
    ; show_hint : bool
    }
  [@@deriving sexp, fields, compare]

  let init mode =
    let show_hint =
      match mode with
      | GameEngine.Normal _ -> false
      | GameEngine.Shuffle -> true
    in
    { game = GameEngine.new_game mode; input = ""; show_hint }
  ;;

  let get_default () =
    let todays_game = init (GameEngine.todays_game ()) in
    let loaded =
      Option.value
        ~default:todays_game
        (LocalStorage.load_model "model" ~deserializer:t_of_sexp)
    in
    if GameEngine.is_todays_game loaded.game then loaded else todays_game
  ;;

  let enter_word t =
    if GameEngine.validate_word t.game (String.lowercase t.input)
    then (
      let g = GameEngine.enter_word t.game (String.lowercase t.input) in
      let t = { t with game = g; input = "" } in
      if GameEngine.is_normal t.game
      then LocalStorage.save_model ~model:t ~serializer:sexp_of_t "model";
      t)
    else t
  ;;

  let update_input t w = { t with input = w }
  let reset_game m = init (GameEngine.mode m.game)

  let switch_gamemode m =
    if GameEngine.is_normal m.game then init GameEngine.Shuffle else get_default ()
  ;;

  let set_show_hint m =
    let m = { m with show_hint = true } in
    if GameEngine.is_normal m.game
    then LocalStorage.save_model ~model:m ~serializer:sexp_of_t "model";
    m
  ;;

  let cutoff t1 t2 = compare t1 t2 = 0
end

module Action = struct
  type t =
    | Enter_word
    | Enter_char of char
    | Backspace
    | Reset_game
    | Switch_mode
    | Show_hint
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
          | GameEngine.Correct c -> c, "#538d4e"
          | GameEngine.In_word c -> c, "#b59f3b"
          | GameEngine.Incorrect c -> c, "white"
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
    let%map start_txt = m >>| Model.game >>| GameEngine.start >>| String.uppercase in
    Node.div
      ~attr:(Attr.many_without_merge [ Attr.style (Css_gen.color (`Name "white")) ])
      [ Node.text start_txt ]
  and guess_list =
    let%map guesses = m >>| Model.game >>| GameEngine.guesses in
    List.map ~f:build_word guesses |> List.rev
  and input =
    let%map input_text = m >>| Model.input in
    Node.div
      ~attr:(Attr.many_without_merge [ Attr.style (Css_gen.color (`Name "#b59f3b")) ])
      [ Node.text (input_text ^ "\u{200B}") ]
  and target_label =
    let%map locked = m >>| Model.game >>| GameEngine.locked_in_letters
    and show_hint = m >>| Model.show_hint in
    if show_hint then build_word ~attrs:[ Attr.id "target" ] locked else Node.div []
  and shuffle_icon =
    let%map is_normal = m >>| Model.game >>| GameEngine.is_normal in
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
    and is_normal = m >>| Model.game >>| GameEngine.is_normal in
    if (not show_hint) && is_normal
    then
      Node.button
        ~attr:
          (Attr.many_without_merge
             [ Attr.id "hint-btn"; Attr.on_click (fun _ -> inject Action.Show_hint) ])
        [ Node.text "Show hint" ]
    else Node.div []
  and reset_button =
    let%map is_normal = m >>| Model.game >>| GameEngine.is_normal in
    if not is_normal
    then
      Node.button
        ~attr:
          (Attr.many_without_merge
             [ Attr.id "reset"; Attr.on_click (fun _ -> inject Action.Reset_game) ])
        [ Node.text "New Game" ]
    else Node.div []
  and is_over = m >>| Model.game >>| GameEngine.game_over in
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
