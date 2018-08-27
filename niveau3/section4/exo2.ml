let read_and_sort_books = fun i ->
  let rec read_and_sort_books_once = fun acc i ->
    if i <= 0 then
      acc
    else
      read_and_sort_books_once (List.merge String.compare [(input_line stdin)] acc) (i-1) in
  read_and_sort_books_once [] i

let _ =
  let nb_books = int_of_string (input_line stdin) in
  List.iter (fun x -> print_endline x) (read_and_sort_books nb_books)
