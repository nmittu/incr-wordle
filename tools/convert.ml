let convert_file filename =
  let file = open_in filename in
  let rec read_f acc =
    try
      let line = input_line file in
      let prefix = if String.length acc <= 1 then "\"" else ";\"" in
      read_f (acc ^ prefix ^ line ^ "\"")
    with
    | _ -> acc
  in
  read_f "[" ^ "]"
;;

convert_file Sys.argv.(1) |> print_string;;
print_endline
