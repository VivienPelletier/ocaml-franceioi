let read_line() = input_line stdin

let print_list = fun l ->
  Queue.iter (fun x -> print_endline x) l

let read_and_filter_books = fun i ->
  let q = Queue.create() in
  let rec read_and_filter_books_once = fun prev i ->
    if i > 0 then
      (
        let current_book = read_line() in
        if Queue.is_empty q then
          (
            Queue.push current_book q;
            read_and_filter_books_once current_book (i-1)
          )
        else
            if String.compare prev current_book < 0 then
              (
                Queue.push current_book q;
                read_and_filter_books_once current_book (i-1)
              )
            else
              read_and_filter_books_once prev (i-1)
          
      ) in (* end if counter isnt out *)
  read_and_filter_books_once "" i;
  q

let _ =
  let nb_books = int_of_string (read_line()) in
  print_list (read_and_filter_books nb_books)
