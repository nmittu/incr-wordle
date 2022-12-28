open Core
open Incr_dom
open Game_engine
module SolutionMap = Map.Make (Int)

let view ~is_closed ~game ~solution_history ~close_stats =
  let open Incr.Let_syntax in
  let open Vdom in
  let%map is_closed = is_closed
  and game_over = game >>| GameEngine.game_over
  and score = game >>| GameEngine.guesses >>| List.length
  and mean =
    solution_history
    >>| fun solution_history ->
    let games, score =
      SolutionMap.fold
        solution_history
        ~init:(0, 0)
        ~f:(fun ~key ~data (num_games, tot_score) ->
          num_games + data, tot_score + (key * data))
    in
    Float.of_int score /. Float.of_int games
  in
  if is_closed || not game_over
  then Node.div []
  else
    Node.div
      ~attr:(Attr.many_without_merge [ Attr.class_ "popup-cont" ])
      [ Node.div
          ~attr:(Attr.many_without_merge [ Attr.class_ "popup" ])
          [ Node.h3 [ Node.text "You Won!" ]
          ; Node.div [ Node.text ("Score: " ^ Int.to_string score) ]
          ; Node.div [ Node.text ("Average Score: " ^ Float.to_string mean) ]
          ; Node.button
              ~attr:(Attr.many_without_merge [ Attr.on_click close_stats ])
              [ Node.text "Close" ]
          ]
      ]
;;
