open Core
open Incr_dom
open Game_engine

module Model = struct
  type t = {
    game: GameEngine.t;
    locked_history: bool list list;
    input: string
  }
  [@@deriving sexp, fields, compare]

  let init () = {
    game=GameEngine.new_game();
    locked_history=[];
    input=""
  }

  let enter_word t =
    if GameEngine.validate_word t.game (String.lowercase t.input) then
      let g = GameEngine.enter_word t.game (String.lowercase t.input) in
      let locked = List.map ~f:(fun o -> Option.is_some o) (GameEngine.locked_in_letters g) in
      {
        game = g;
        locked_history = locked::t.locked_history;
        input = "";
      }
    else t

  let update_input t w =
    {t with input=w}

  let reset_game _ = init ()
    
  let cutoff t1 t2 = compare t1 t2 = 0
end

module Action = struct
  type t =
    | Enter_word
    | Enter_char of char
    | Backspace
    | Reset_game
  [@@deriving sexp]
end

module State = struct
  type t = unit
end

let apply_action model action _ ~schedule_action:_ =
  match (action : Action.t) with
  | Enter_word -> Model.enter_word model 
  | Enter_char c -> Model.update_input model (model.input ^ (Char.to_string c))
  | Backspace -> let input = Model.input model in 
      if String.(=) input "" then model 
      else if String.length input = 1 then
        Model.update_input model ""
      else 
        Model.update_input model (String.slice (Model.input model) 0 (String.length (Model.input model) - 1))
  | Reset_game -> Model.reset_game model

let on_startup ~schedule_action:_ _ = Async_kernel.return ()

let handle_keyup keycode ~inject =
  if keycode >= 65 && keycode <= 90 then
    match Char.of_int keycode with
    | Some c -> inject (Action.Enter_char c)
    | None -> failwith "impossible"
  else if keycode = 8 then
    inject Action.Backspace
  else if keycode = 13 then
    inject Action.Enter_word
  else Ui_effect.return();;

let explode s = List.init (String.length s) ~f:(String.get s)

let build_word ~locked ?(attrs=[]) w =
  let open Vdom in
  let spans = List.map 
    ~f:(fun (c, l) -> 
      let color = if l then "green" else "black" in
      Node.span 
        ~attr:
          (Attr.many_without_merge
            ([Attr.style (Css_gen.color (`Name color))] @ attrs))
        [Node.text (Char.to_string c)]) 
    (List.zip_exn (w |> String.uppercase |> explode) locked) in
  Node.div spans



let view (m: Model.t Incr.t) ~inject = 
  let open Incr.Let_syntax in
  let open Vdom in

  let keyboard = Keyboard.gen_keyboard
    ~on_click:(fun c -> inject (Action.Enter_char c))
    ~on_delete:(fun () -> inject Action.Backspace)
    ~on_enter:(fun () -> inject Action.Enter_word) in

  let reset_button =
    Node.button
      ~attr:(Attr.many_without_merge
        [ Attr.id "reset"
        ; Attr.on_click (fun _ -> inject Action.Reset_game)])
      [Node.text "New Game"] in

  let%map start_label = 
    let%map start_txt = m >>| Model.game >>| GameEngine.start >>| String.uppercase in
    Node.div [ Node.text start_txt ]

  and guess_list =
    let%map guesses = m >>| Model.game >>| GameEngine.guesses 
    and locked = m >>| Model.locked_history in 
    List.map ~f:(fun (w, locked) -> build_word ~locked w) (List.zip_exn guesses locked) |> List.rev

  and input =
    let%map input_text = m >>| Model.input in
    Node.div 
      ~attr:(Attr.many_without_merge
        [Attr.style (Css_gen.color (`Name "chocolate"))])
      [Node.text (input_text^"\u{200B}")]
  
  and target_label = 
    let%map target_text = m >>| Model.game >>| GameEngine.target >>| String.uppercase
    and locked = m >>| Model.game >>| GameEngine.locked_in_letters in
    build_word 
      ~locked:(List.map ~f:Option.is_some locked) 
      ~attrs:[Attr.id "target"]
      target_text 
    
  and is_over = m >>| Model.game >>| GameEngine.game_over in

  let input_target = if is_over then [target_label; reset_button] else [input; target_label] in

  Node.body 
    ~attr:
      (Attr.many_without_merge
        [Attr.on_keyup (fun e -> handle_keyup e##.keyCode ~inject)])
    ([
      Node.div 
        ~attr:(Attr.many_without_merge
          [Attr.id "game"])
        ([start_label] @ guess_list @ input_target);
      keyboard
    ]);;


let on_display ~old_model model _ ~schedule_action:_ = 
  if Model.compare old_model model <> 0 then
    Js_misc.scroll ~id:"keyboard-cont" ()
  else ()

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
    on_display ~old_model model in
  Component.create ~apply_action ~on_display model view;;

let initial_model = Model.init ();;

  