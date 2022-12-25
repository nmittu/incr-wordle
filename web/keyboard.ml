open Core
open Incr_dom

let layout = [
  [ "Q"; "W"; "E"; "R"; "T"; "Y"; "U"; "I"; "O"; "P"]
; [ "A"; "S"; "D"; "F"; "G"; "H"; "J"; "K"; "L"]
; [ "Enter"; "Z"; "X"; "C"; "V"; "B"; "N"; "M"; "Del"]
]

let gen_keyboard ~on_click ~on_delete ~on_enter = 
  let open Vdom in
  let rows = List.map 
    ~f:(fun r -> 
      Node.div (
      List.map ~f:(fun c -> 
        Node.button
          ~attr:(
            Attr.many_without_merge
              [ Attr.class_ "keyboard-button"
              ; Attr.type_ "button"
              ; Attr.on_keydown (fun e -> 
                Effect.Many
                  [ Effect.Prevent_default 
                  ; if e##.keyCode = 13 then
                      on_enter()
                    else Effect.return()])
              ; Attr.on_click (fun _ -> 
                if String.(=) c "Del" then
                  on_delete()
                else if String.(=) c "Enter" then
                  on_enter()
                else on_click (String.get c 0))])
          [Node.text c])
      r))
    layout in
  
  Node.div
    ~attr:(Attr.many_without_merge [Attr.id "keyboard-cont"])
    rows