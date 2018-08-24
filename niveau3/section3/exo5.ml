let is_palindrome = fun s ->
  let s = String.lowercase_ascii s in
  let rec is_palindrome = fun i j ->
    if i >= j then
      true
    else if s.[i] = ' ' then
      is_palindrome (i+1) j
    else if s.[j] = ' ' then
      is_palindrome i (j-1)
    else if s.[i] = s.[j] then
            is_palindrome (i+1) (j-1)
    else
      false in
  is_palindrome 0 ((String.length s)-1)

let _ = 
  let nbLivres = int_of_string (input_line stdin) in
  for i=1 to nbLivres do
    let nom = input_line stdin in
    if is_palindrome nom then
      print_endline nom
  done;
