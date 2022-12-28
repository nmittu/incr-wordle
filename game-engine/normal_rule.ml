open! Core
open Game_engine

module Rule = struct
  type t = int [@@deriving sexp, compare]

  let hash x =
    let x = Int.shift_right x 16 lxor x * 0x45d9f3b in
    let x = Int.shift_right x 16 lxor x * 0x45d9f3b in
    let x = Int.shift_right x 16 lxor x in
    x
  ;;

  let gen2 t range =
    let h = hash t in
    let x = h land 0xffff
    and y = Int.shift_right h 16 in
    x mod range, y mod range
  ;;

  let get_start_end t =
    let st, tg = gen2 t (Array.length Words.possible_words) in
    Words.possible_words.(st), Words.possible_words.(tg)
  ;;

  let get_hints _ ~target ~word =
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
    let correct, lr = gen_correct (explode word) (explode target) in
    let in_w = gen_inword (explode word) lr in
    merge correct in_w
  ;;

  let get_new_target _ ~locked_in:_ ~old_target = old_target
  let get_today = Get_date.get_date_int
end
