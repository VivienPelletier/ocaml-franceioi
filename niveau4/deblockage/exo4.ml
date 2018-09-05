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

let compare_coat = fun (min1, max1) (min2, max2) ->
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

let is_include_coat = fun (min1, max1) (min2, max2, _) ->
  (min1 >= min2 && max1 <= max2)

let levelize_coats = fun coats ->
  let rec merge_coat = fun (min1, max1) coats ->
    match coats with
    | [] -> []
    | (min2,max2,nb2)::tl -> if min1 < min2 || max1 > max2 then
        coats
      else
        (min2,max2,nb2+1) :: (merge_coat (min1, max1) tl) in
  let rec levelize_coats_rec = fun coats levelized_coats ->
    match coats with
    | [] -> levelized_coats
    | current_coat::tl -> match levelized_coats with
      | [] -> let min, max = current_coat in
        levelize_coats_rec tl [(min, max, 0)]
      | other_coat::_ -> 
        let new_levelized_coat =
          if is_include_coat current_coat other_coat then
            merge_coat current_coat levelized_coats
          else
            let min, max = current_coat in
            (min,max,0) :: levelized_coats in
        levelize_coats_rec tl new_levelized_coat in
  let coats = List.sort compare_coat coats in
  levelize_coats_rec coats []
    
let max_level_coats = fun coats ->
  let max_level_coats_fold = fun max (_,_,x) ->
    if max >= x then
      max
    else
      x in
  match coats with
  | [] -> failwith "empty list"
  | (_,_,nb)::tl -> 
    List.fold_left max_level_coats_fold nb tl

let _ = let coats = read_coats() in
  let coats = levelize_coats coats in
  Printf.printf "%d\n" (max_level_coats coats)
