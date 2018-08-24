let line = input_line stdin

let count_ocr = fun s ->
  let counts = Array.make 26 0 in
  let count_ocr_once = fun c ->
    if c >= 'a' && c <= 'z' then
      let i = (Char.code c) - (Char.code 'a') in
      counts.(i) <- counts.(i) + 1
    else if c >= 'A' && c <= 'Z' then
      let i = (Char.code c) - (Char.code 'A') in
      counts.(i) <- counts.(i) + 1 in
  String.iter count_ocr_once s;
  counts

let freq = fun s ->
  let sum_once = fun acc x ->
    acc+x in
  let ocrs = count_ocr s in
  let sum = Array.fold_left sum_once 0 ocrs in
  let freq_once = fun x ->
    (float_of_int x) /. (float_of_int sum) in
  Array.map freq_once ocrs

let _ = Array.iter (fun x -> Printf.printf "%f\n" x) (freq line)
