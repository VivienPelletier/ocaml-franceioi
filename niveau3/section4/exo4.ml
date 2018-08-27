let read_pair = fun () ->
  let line = input_line stdin in
  Scanf.sscanf line "%s %s" (fun x y -> x, y)

let read_int = fun () ->
  int_of_string (input_line stdin)
    
let print_list = fun l ->
  List.iter (fun (x, y) -> Printf.printf "%s %s\n" x y) l

let read_words = fun i ->
  let rec read_words_once = fun acc i ->
    if i <= 0 then
      acc
    else
      let s1, s2 = read_pair() in
      read_words_once ((s2,s1)::acc) (i-1) in
  read_words_once [] i

let compare_assoc = fun (s1, _) (s2, _) ->
  String.compare s1 s2

let _ =
  let nb_words = read_int() in
  let words = read_words nb_words in
  print_list (List.sort compare_assoc words)
