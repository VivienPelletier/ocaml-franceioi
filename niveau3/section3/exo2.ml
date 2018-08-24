let nom1 = Scanf.scanf " %s" (fun x -> x)
let nom2 = Scanf.scanf " %s" (fun x -> x)

let sum_char = fun s ->
  let sum = ref 0 in
  let sum_char_once = fun c ->
    sum := !sum + (Char.code c - Char.code 'A') in
  String.iter sum_char_once s;
  !sum

let sum_digit = fun x ->
  let rec sum_digit_rec = fun x acc ->
    if x < 10 then
      let sum = acc + x in
      if sum < 10 then
        sum
      else
        sum_digit_rec sum 0
    else
      sum_digit_rec (x / 10) (x mod 10 + acc) in
  sum_digit_rec x 0

let love_number = fun s ->
  sum_digit (sum_char s)

let _ = Printf.printf "%d %d\n" (love_number nom1) (love_number nom2);
