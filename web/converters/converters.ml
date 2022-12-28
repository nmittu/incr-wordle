let converters = [ Convert_12_29_2022.convert ]

let convert top_converter sexp =
  let top_converter sexp =
    try Some (top_converter sexp) with
    | _ -> None
  in
  let rec convert_help converters =
    match converters with
    | c :: tl ->
      let opt = c sexp in
      (match opt with
      | Some m -> Some m
      | None -> convert_help tl)
    | [] -> None
  in
  convert_help (top_converter :: converters)
;;
