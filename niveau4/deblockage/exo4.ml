let read_int = fun () ->
  Scanf.scanf " %d" (fun x -> x)

let read_2_int = fun () ->
  Scanf.scanf " %d %d" (fun x y -> x, y)

let read_coats = fun () ->
  let nb_coat = read_int() in
  let rec read_coats_rec = fun i acc ->
    if i = 0 then
      acc
    else
      let min, max = read_2_int() in
      read_coats_rec (i-1) ((min, max)::acc) in
  read_coats_rec nb_coat []

let compare_coat_by_min = fun (min1, max1) (min2, max2) ->
  if min1 < min2 then
    -1
  else if min1 > min2 then
    1
  else if max1 > max2 then
    -1
  else if max1 < max2 then
    1
  else
    0

let compare_coat_by_max = fun (min1, max1, _) (min2, max2, _) ->
  if max1 > max2 then
    -1
  else if max1 < max2 then
    1
  else if min1 < min2 then
    -1
  else if min1 > min2 then
    1
  else
    0

let compute_max_level = fun coats ->
  let compute_position_mapi = fun i (min, max) ->
    (min,max,i) in
  let compute_level_mapi = fun p2 (_, _, p1) ->
    List.length coats - p1 - p2 - 1 in
  let compute_max_fold = fun max_level curr_level ->
    if curr_level > max_level then
      curr_level else
      max_level in
  let coats = List.sort compare_coat_by_min coats in
  let coats = List.mapi compute_position_mapi coats in
  let coats = List.sort compare_coat_by_max coats in
  let coats_levels = List.mapi compute_level_mapi coats in
  match coats_levels with
  | [] -> failwith "empty list"
  | hd_level::tl -> List.fold_left compute_max_fold hd_level tl 

let _ = let coats = read_coats() in
  let max_level = compute_max_level coats in
  Printf.printf "%d\n" max_level
