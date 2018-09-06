let read_int = fun () ->
  Scanf.scanf " %d" (fun x -> x)

let read_2int = fun () ->
  Scanf.scanf " %d %d" (fun x y -> x, y)

let read_3int = fun () ->
  Scanf.scanf " %d %d %d" (fun x y z -> x, y, z)

let read_coord_trees = fun nb_trees ->
  Array.init nb_trees (fun _ -> read_3int())

let is_in_radius = fun (x1, y1, r1) (x2, y2, _) ->
  let x = x1 - x2 and y = y1 - y2 in
  (x*x + y*y) <= r1*r1

let gen_graph = fun coord_trees ->
  let gen_adj_mapi = fun icurr_tree curr_coord ->
    let gen_adj_fold = fun (i, acc) other_coord ->
      if i = icurr_tree then
        (i+1,acc)
      else if is_in_radius curr_coord other_coord then
        (i+1,i::acc)
      else
        (i+1,acc) in
    let _, l = Array.fold_left gen_adj_fold (0,[]) coord_trees in
    l in
  Array.mapi gen_adj_mapi coord_trees

let spread_sickness = fun adj_graph ->
  let count_sick = fun sick_trees ->
    let nb_sick = ref 0 in
    let count_sick_iter = fun b ->
      if b then incr nb_sick in
    Array.iter count_sick_iter sick_trees;
    !nb_sick in
  let rec spread = fun sick_trees next ->
    let spread_fold = fun acc i ->
      if not sick_trees.(i) then
        begin
          sick_trees.(i) <- true;
          adj_graph.(i) @ acc;
        end
      else
        acc in
    match next with 
    | [] -> ()
    | _ -> spread sick_trees
             (List.fold_left spread_fold [] next) in
  let spead_init = fun sick_tree -> 
    let sick_trees = Array.make (Array.length adj_graph) false in
    sick_trees.(sick_tree) <- true;
    spread sick_trees (adj_graph.(sick_tree));
    count_sick sick_trees in
  Array.init (Array.length adj_graph) spead_init 

let resolve_requests = fun nb_requests spreaded_sickness ->
  for i=1 to nb_requests do
    Printf.printf "%d\n" (spreaded_sickness.(read_int()))
  done

let _ =
  let nb_trees, nb_requests = read_2int() in
  let coord_trees = read_coord_trees nb_trees in
  let adj_graph = gen_graph coord_trees in
  let spreaded_sickness = spread_sickness adj_graph in
  resolve_requests nb_requests spreaded_sickness
