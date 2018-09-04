let read_int = fun () ->
  Scanf.scanf " %d" (fun x -> x)

let show_int = fun i ->
  Printf.printf "%d\n" i

let solve_baguenaudier = fun i ->
  let rec empty_baguenaudier_rec = fun i ->
    if i = 1 then
      show_int 1
    else if i = 2 then
      begin
        show_int 2;
        show_int 1;
      end
    else
      begin
        empty_baguenaudier_rec (i-2);
        show_int i;
        fill_baguenaudier_rec (i-2);
        empty_baguenaudier_rec (i-1);
      end 
   and fill_baguenaudier_rec = fun i ->
      if i = 1 then
        show_int 1
      else if i = 2 then
        begin
          show_int 1;
          show_int 2;
        end
      else
        begin
          fill_baguenaudier_rec (i-1);
          empty_baguenaudier_rec (i-2);
          show_int i;
          fill_baguenaudier_rec (i-2);
        end;
    in
    empty_baguenaudier_rec i 

let _ =
  let n = read_int() in
  solve_baguenaudier n
