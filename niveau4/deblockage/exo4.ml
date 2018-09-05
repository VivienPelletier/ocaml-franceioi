let read_int = fun () ->
  Scanf.scanf " %d" (fun x -> x)

let read_2_int = fun () ->
  Scanf.scanf " %d %d" (fun x y -> x, y)

type coat_t = { 
  min : int; 
  max : int; 
  mutable p1 : int; }

let read_coats = fun () ->
  let nb_coat = read_int() in
  let read_coat = fun i ->
      let min, max = read_2_int() in
      {min=min; max=max; p1=0} in
  Array.init nb_coat read_coat

let compare_coat_by_min = fun c1 c2 ->
  if c1.min < c2.min then
    -1
  else if c1.min > c2.min then
    1
  else if c1.max > c2.max then
    -1
  else if c1.max < c2.max then
    1
  else
    0

let compare_coat_by_max = fun c1 c2 ->
  if c1.max > c2.max then
    -1
  else if c1.max < c2.max then
    1
  else if c1.min < c2.min then
    -1
  else if c1.min > c2.min then
    1
  else
    0

let compute_max_level = fun coats ->
  let compute_position_iteri = fun i coat ->
    coat.p1 <- i in
  let compute_level_mapi = fun p2 coat ->
    Array.length coats - coat.p1 - p2 - 1 in
  let compute_max_fold = fun max_level curr_level ->
    if curr_level > max_level then
      curr_level 
    else
      max_level in
  Array.sort compare_coat_by_min coats;
  Array.iteri compute_position_iteri coats;
  Array.stable_sort compare_coat_by_max coats;
  let coats_levels = Array.mapi compute_level_mapi coats in
  if Array.length coats_levels < 1 then
    failwith "empty list"
  else
    Array.fold_left compute_max_fold coats_levels.(0) coats_levels 

let _ = let coats = read_coats() in
  let max_level = compute_max_level coats in
  Printf.printf "%d\n" max_level
