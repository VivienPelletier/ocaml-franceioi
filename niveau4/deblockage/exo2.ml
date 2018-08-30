let print_list = fun l ->
  List.iter (fun x -> Printf.printf "%d " x) l;
  print_newline()

let biggest_smaller_fact = fun x ->
  let rec biggest_smaller_fact_rec = fun acc i ->
    if acc*i > x then
      (acc, i-1)
    else
      biggest_smaller_fact_rec (acc*i) (i+1) in
  biggest_smaller_fact_rec 1 1

let boxes = fun boxe_size i x ->
  let rec boxes_rec = fun boxe_size i x acc ->
    if i <= 0 then
      acc
    else
      let nb_boxes = x/boxe_size in
      boxes_rec (boxe_size/i) (i-1) (x mod boxe_size) (nb_boxes :: acc) in
  boxes_rec boxe_size i x []

let _ = 
  let nb_peas = Scanf.scanf " %d" (fun x -> x) in
  let biggest_boxe_size, p = biggest_smaller_fact nb_peas in
  let boxes_needed = boxes biggest_boxe_size p nb_peas in
  Printf.printf "%d\n" p;
  print_list boxes_needed
