open! Core
open Game_engine

module Rule = struct
  type t = unit [@@deriving sexp, compare]

  let getword words =
    let n = Random.int (Array.length words) in
    words.(n)
  ;;

  let get_start_end () = 
    let rec get_start_target () =
      let start, target = getword Words.possible_words, getword Words.possible_words in
      if word_diff start target > 1 then start, target else get_start_target ()
    in get_start_target ()

  let get_hints () ~target ~word =
    List.map
      ~f:(fun (w, t) -> if Char.(w = t) then Correct w else Incorrect w)
      (List.zip_exn (explode word) (explode target))

  let get_new_target () ~locked_in ~old_target:_ =
    let new_possibilites =
      Array.filter
        ~f:(fun w ->
          List.fold
            ~f:(fun b (l, c) ->
              match l with
              | Correct c1 -> b && Char.( = ) c1 c
              | _ -> b)
            ~init:true
            (List.zip_exn locked_in (explode w)))
        Words.all_words
    in getword new_possibilites
end