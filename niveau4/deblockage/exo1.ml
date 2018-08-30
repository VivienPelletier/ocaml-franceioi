let read_line = fun () ->
  input_line stdin

let min3 = fun x y z ->
  if x > y then
    if y > z then
      z
    else
      y
  else if x > z then
    z
  else
    x

let max_matrix = fun m ->
  let max_line = fun max l ->
    let max_once = fun max v ->
      if max < v then
        v
      else
        max in
    let new_max = Array.fold_left max_once (l.(0)) l in
    if max < new_max then
      new_max
    else
      max in
  Array.fold_left max_line (m.(0).(0)) m

let read_matrix = fun () ->
  let nb_lines, nb_cols = 
    Scanf.sscanf (read_line()) "%d %d" (fun x y -> x, y) in
  let matrix = Array.init nb_lines ( fun l ->
      Array.init nb_cols ( fun c ->
          Scanf.scanf " %d" (fun x -> x))) in
  matrix

let largest_square = fun m ->
  let largest_square_one_line = fun l t ->
    let largest_square_once = fun c v ->
      if c = 0 then
        if v = 0 then 
          m.(l).(c) <- 1 
        else 
          m.(l).(c) <- 0
      else if v = 0 then 
        m.(l).(c) <- 1 + min3 (m.(l-1).(c-1)) (m.(l-1).(c)) (m.(l).(c-1))
      else
        m.(l).(c) <- 0
    in
    if l = 0 then
      Array.iteri (fun c v -> 
          if v = 0 then 
            m.(l).(c) <- 1
          else
            m.(l).(c) <- 0) t
    else
      Array.iteri largest_square_once t
  in
  Array.iteri largest_square_one_line m

let _ = 
  let matrix = read_matrix() in
  largest_square matrix; 
  Printf.printf "%d\n" (max_matrix matrix)
