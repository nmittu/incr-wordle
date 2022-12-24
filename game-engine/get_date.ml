let get_date_int () =
  let date = Unix.gmtime (Unix.time ()) in
  date.tm_year * 100 + date.tm_yday