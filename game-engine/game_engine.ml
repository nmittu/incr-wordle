open Core

module GameEngine = struct
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
  [@@deriving sexp, compare]

  let todays_game () = Normal (Get_date.get_date_int ())

  let is_todays_game g =
    match g.game_mode with
    | Normal id -> id = Get_date.get_date_int ()
    | _ -> false
  ;;

  let get_generator =
    let open Number_generators in
    function
    | Shuffle -> (module RandomGenerator : NumberGenerator)
    | Normal id ->
      let module G = DeterministicGenerator () in
      G.init id;
      (module G)
  ;;

  let getword gen words =
    let n = gen (Array.length words) in
    words.(n)
  ;;

  let get_two_word gen words =
    let n1, n2 = gen (Array.length words) in
    words.(n1), words.(n2)
  ;;

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

  let new_game game_mode =
    let module Gen = (val get_generator game_mode) in
    let rec get_start_target () =
      let start, target = get_two_word Gen.gen2 Words.possible_words in
      if word_diff start target > 1 then start, target else get_start_target ()
    in
    let start, target = get_start_target () in
    { game_mode; start; guesses = []; target }
  ;;

  let is_normal g =
    match g.game_mode with
    | Normal _ -> true
    | _ -> false
  ;;

  let start g = g.start
  let target g = g.target
  let mode g = g.game_mode

  let rec letter_hints_to_s hints =
    match hints with
    | Incorrect c :: tl -> Char.to_string c ^ letter_hints_to_s tl
    | Correct c :: tl -> Char.to_string c ^ letter_hints_to_s tl
    | In_word c :: tl -> Char.to_string c ^ letter_hints_to_s tl
    | [] -> ""
  ;;

  let locked_in_letters g =
    match g.guesses with
    | w :: _ ->
      List.map2_exn
        ~f:(fun a b ->
          match a, b with
          | Correct _, w -> Correct w
          | Incorrect _, w -> Incorrect w
          | In_word _, w -> In_word w)
        w
        (explode g.target)
    | [] -> List.map ~f:(fun c -> Incorrect c) (explode g.target)
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

  let validate_word g w =
    let w = String.lowercase w in
    let diff = word_diff w (get_prev g) in
    (not (game_over g))
    && (diff = 1 || (String.( = ) w g.target && diff = 0))
    && Array.find ~f:(fun x -> String.( = ) x w) Words.all_words |> Option.is_some
  ;;

  let get_locked w1 w2 =
    let rec help w1 w2 =
      match w1, w2 with
      | c1 :: t1, c2 :: t2 when Char.( = ) c1 c2 -> Some c1 :: help t1 t2
      | _ :: t1, _ :: t2 -> None :: help t1 t2
      | [], _ | _, [] -> []
    in
    help (explode w1) (explode w2)
  ;;

  let get_hints g w =
    let target = g.target in
    if is_normal g
    then (
      let rec replace_first l x ~with_e =
        match l with
        | h :: t when Char.(h = x) -> with_e :: t
        | h :: t -> h :: replace_first t x ~with_e
        | [] -> []
      in
      let rec gen_correct w t =
        match w, t with
        | w :: wt, t :: tt when Char.(w = t) ->
          let hr, lr = gen_correct wt tt in
          `Correct w :: hr, ' ' :: lr
        | w :: wt, t :: tt ->
          let hr, lr = gen_correct wt tt in
          `Incorrect w :: hr, t :: lr
        | [], _ | _, [] -> [], []
      in
      let rec gen_inword w lr =
        match w with
        | h :: t ->
          if Option.is_some (List.find lr ~f:(Char.( = ) h))
          then `In_word h :: gen_inword t (replace_first lr h ~with_e:' ')
          else `Incorrect h :: gen_inword t lr
        | [] -> []
      in
      let rec merge c iw =
        match c, iw with
        | `Correct c :: ct, _ :: it -> Correct c :: merge ct it
        | _ :: ct, `In_word c :: it -> In_word c :: merge ct it
        | `Incorrect c :: ct, `Incorrect _ :: it -> Incorrect c :: merge ct it
        | [], _ | _, [] -> []
      in
      let correct, lr = gen_correct (explode w) (explode target) in
      let in_w = gen_inword (explode w) lr in
      merge correct in_w)
    else
      List.map
        ~f:(fun (w, t) -> if Char.(w = t) then Correct w else Incorrect w)
        (List.zip_exn (explode w) (explode target))
  ;;

  let enter_word g w =
    let locked_in = get_locked g.target w in
    let new_possibilites =
      Array.filter
        ~f:(fun w ->
          List.fold
            ~f:(fun b (l, c) ->
              match l with
              | Some c1 -> b && Char.( = ) c1 c
              | None -> b)
            ~init:true
            (List.zip_exn locked_in (explode w)))
        Words.all_words
    in
    let target =
      match g.game_mode with
      | Shuffle ->
        let module Gen = (val get_generator g.game_mode) in
        getword Gen.gen new_possibilites
      | Normal _ -> g.target
    in
    if validate_word g w
    then
      if String.( = ) w g.target
      then { g with guesses = get_hints g w :: g.guesses }
      else { g with start = g.start; guesses = get_hints g w :: g.guesses; target }
    else failwith "Invalid word"
  ;;

  let guesses g = g.guesses
end
