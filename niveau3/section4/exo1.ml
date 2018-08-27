let read_books_title = fun i ->
  let rec read_books_title_once = fun acc i ->
  if i <= 0 then 
    acc
  else
    read_books_title_once ((input_line stdin)::acc) (i-1) in
  read_books_title_once [] i

let _ =
  let nb_books = int_of_string (input_line stdin) in
  let books = read_books_title nb_books in
  List.iter (fun x -> print_endline x) books
