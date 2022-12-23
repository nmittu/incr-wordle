open Core

module GameEngine = struct
  type t = {
    start: string;
    guesses: string list;
    target: string;
    locked_in: char option list;
  }
  [@@deriving sexp, compare]

  let randelt words = 
    let n = Random.int (List.length words) in
    match List.nth words n with
    | Some w -> w
    | None -> failwith "Impossible";;

  
  let explode s = List.init (String.length s) ~f:(String.get s)

  let word_diff w1 w2 =
    let w1, w2 = (explode w1, explode w2) in
    let rec calc_diff w1 w2 =
      match w1, w2 with
      | c1::t1, c2::t2 when Char.(=) c1 c2 -> calc_diff t1 t2
      | _::t1, _::t2 -> 1 + calc_diff t1 t2
      | _, [] | [], _ -> 0 in
    calc_diff w1 w2;;

  let new_game () =
    let rec get_start_target () = 
      let start = randelt Words.words in
      let target = randelt Words.words in
      if word_diff start target > 1 then
        start, target
      else get_start_target() in
    let start, target = get_start_target () in
    {
      start;
      guesses= [];
      target;
      locked_in= [None; None; None; None; None];
    }

  let start g = g.start

  let target g = g.target

  let game_over g = match g.guesses with
  | w::_ when String.(=) w g.target -> true && (List.fold ~f:(fun a x -> a&&(Option.is_some x)) ~init:true g.locked_in)
  | _ -> false

  let get_prev g = match g.guesses with
  | w::_ -> w
  | [] -> g.start

  let validate_word g w =
    let w = String.lowercase w in
    let diff = word_diff w (get_prev g) in
    (not (game_over g)) &&
    (diff = 1 || (String.(=) w g.target && diff = 0)) && List.find ~f:(fun x -> String.(=) x w) Words.words |> Option.is_some

  let get_locked w1 w2 = 
    let rec help w1 w2 = 
      match w1, w2 with
      | c1::t1, c2::t2 when Char.(=) c1 c2 -> (Some c1)::help t1 t2
      | _::t1, _::t2 -> None::help t1 t2
      | [], _ | _, [] -> []
    in help (explode w1) (explode w2)

  let enter_word g w = 
    let locked_in = get_locked g.target w in
    let new_possibilites = List.filter 
      ~f:(fun w -> List.fold 
        ~f:(fun b (l, c) -> match l with
          | Some c1 -> b && Char.(=) c1 c
          | None -> b) 
        ~init:true (List.zip_exn locked_in (explode w))) 
      Words.words in
    let target = randelt new_possibilites in

    if validate_word g w then
      if String.(=) w g.target then
        {
          g with
          guesses=w::g.guesses;
          locked_in;
        }
      else
        {
          start=g.start;
          guesses=w::g.guesses;
          target=target;
          locked_in;

        }
    else failwith "Invalid word"
  let locked_in_letters g = g.locked_in

  let guesses g = g.guesses
end