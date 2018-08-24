let is_valid_id = fun s ->
  let ok = ref true in
  let is_valid_id_once = fun c ->
    if (c < '0' || c > '9') && (c < 'A' || c > 'Z') && (c < 'a' || c > 'z') && c != '_' then
      ok := false in
  String.iter is_valid_id_once (String.trim s);
  !ok && (s.[0] < '0' || s.[0] > '9')

let _ = let nbId = int_of_string (input_line stdin) in
  for i=1 to nbId do
    let id = input_line stdin in
    if is_valid_id id then
      print_endline "YES"
    else
      print_endline "NO"
  done;
