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

let imax_ocr = fun s ->
  let imax_once = fun (max, imax, i) x ->
    if x > max then
      (x, i, i+1)
    else
      (max, imax, i+1) in
  let ocrs = count_ocr s in
  let (_, i, _) = Array.fold_left imax_once (ocrs.(0), 0, 0) ocrs in
  Char.chr (Char.code 'A' + i)

let _ = Printf.printf "%c\n" (imax_ocr line)
