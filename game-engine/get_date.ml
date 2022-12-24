let get_date_int () =
  let date = Unix.localtime (Unix.time ()) in
  date.tm_year * 1000 + date.tm_yday