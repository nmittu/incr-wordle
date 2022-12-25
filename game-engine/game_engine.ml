open Core

module GameEngine = struct
  type game_mode = 
      Shuffle
    | Normal of int
  [@@deriving sexp, compare]

  type t = {
    game_mode: game_mode;
    start: string;
    guesses: string list;
    target: string;
    locked_in: char option list;
  }
  [@@deriving sexp, compare]

  let todays_game () =
    Normal (Get_date.get_date_int ())

  let is_todays_game g =
    match g.game_mode with
    | Normal id -> id = (Get_date.get_date_int ())
    | _ -> false

  let get_generator = 
    let open Number_generators in
    function
    | Shuffle -> (module RandomGenerator: NumberGenerator)
    | Normal id -> 
      let module G = DeterministicGenerator() in
        G.init id;
        (module G)

  let getword gen words = 
    let n = gen (Array.length words) in
    words.(n)

  let get_two_word gen words = 
    let n1, n2 = gen (Array.length words) in
    words.(n1), words.(n2)

  
  let explode s = List.init (String.length s) ~f:(String.get s)

  let word_diff w1 w2 =
    let w1, w2 = (explode w1, explode w2) in
    let rec calc_diff w1 w2 =
      match w1, w2 with
      | c1::t1, c2::t2 when Char.(=) c1 c2 -> calc_diff t1 t2
      | _::t1, _::t2 -> 1 + calc_diff t1 t2
      | _, [] | [], _ -> 0 in
    calc_diff w1 w2;;

  let new_game game_mode =
    let module Gen = (val get_generator game_mode) in
    let rec get_start_target () = 
      let start, target = get_two_word Gen.gen2 Words.words in
      if word_diff start target > 1 then
        start, target
      else get_start_target() in
    let start, target = get_start_target () in
    {
      game_mode;
      start;
      guesses= [];
      target;
      locked_in= [None; None; None; None; None];
    }

  let is_normal g = match g.game_mode with
    | Normal _ -> true
    | _ -> false
  
  let start g = g.start

  let target g = g.target

  let mode g = g.game_mode

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
    (diff = 1 || (String.(=) w g.target && diff = 0)) && Array.find ~f:(fun x -> String.(=) x w) Words.words |> Option.is_some

  let get_locked w1 w2 = 
    let rec help w1 w2 = 
      match w1, w2 with
      | c1::t1, c2::t2 when Char.(=) c1 c2 -> (Some c1)::help t1 t2
      | _::t1, _::t2 -> None::help t1 t2
      | [], _ | _, [] -> []
    in help (explode w1) (explode w2)

  let enter_word g w = 
    let locked_in = get_locked g.target w in
    let new_possibilites = Array.filter 
      ~f:(fun w -> List.fold 
        ~f:(fun b (l, c) -> match l with
          | Some c1 -> b && Char.(=) c1 c
          | None -> b) 
        ~init:true (List.zip_exn locked_in (explode w))) 
      Words.words in
    let target = match g.game_mode with
    | Shuffle -> 
      let module Gen = (val get_generator g.game_mode) in
      getword Gen.gen new_possibilites
    | Normal _ -> g.target in

    if validate_word g w then
      if String.(=) w g.target then
        {
          g with
          guesses=w::g.guesses;
          locked_in;
        }
      else
        { g with
          start=g.start;
          guesses=w::g.guesses;
          target=target;
          locked_in;

        }
    else failwith "Invalid word"
  let locked_in_letters g = g.locked_in

  let guesses g = g.guesses
end