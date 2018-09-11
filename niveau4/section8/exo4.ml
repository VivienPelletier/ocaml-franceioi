open Scanf;;

type kind_t = Empty | MirrorS | MirrorA | Border
type cell_t = {
  kind : kind_t;
  id : (int*int option) option;
}
type edge_t = {
  id : int;
  dist : int;
}
type adj_t = (edge_t*edge_t option) option

let read_labo = fun () ->
  let curr_id = ref 0 in
  let nb_lines, nb_cols = 
    let line1 = read_line() in
    sscanf line1 " %d %d" (fun l c -> l,c) in
  let cell_of_char = function
    | '.' -> Empty
    | '/' -> MirrorS
    | '\\' -> MirrorA
    | _ ->  failwith "invalid input" in
  let border_line_init = fun i ->
    if i = 0 || i = nb_cols+1 then
      {kind = Empty; id = None}
    else
      begin
        let id = !curr_id in
        incr curr_id;
        {kind = Border; id = Some (id,None)}
      end in
  let scan_lin i =
    if i = 0 || i = nb_lines+1 then
      Array.init (nb_cols+2) border_line_init
    else
      begin
        let line = read_line() in
        let cell_init = fun i ->
          if i = 0 || i = nb_cols+1 then
            begin
              let id = !curr_id in
              incr curr_id;
              {kind = Border; id = Some (id,None)}
            end
          else
            begin
              let k = cell_of_char line.[i-1] in
              match k with
              | Empty -> {kind = Empty; id = None}
              | Border -> failwith "pas de bord ici !"
              | _ -> let id = !curr_id in
                curr_id := !curr_id+2;
                {kind = k; id = Some (id, Some (id+1))}
            end in
        Array.init (nb_cols+2) cell_init
      end in
  let labo = Array.init (nb_lines+2) scan_lin in
  (labo, !curr_id)

let gen_graph = fun labo nb_nodes ->
  let add_edge_to_node = fun id dist node ->
    match node with
    | None -> Some ({id=id; dist=dist}, None)
    | Some (e1, x) -> match x with
      | None -> Some (e1, Some {id=id; dist=dist})
      | _ -> failwith "add edge to full node" in
  let graph = Array.make nb_nodes None in
  let gen_h_node_cell_fold = fun (last_node, dist) cell ->
    match cell.kind with
    | Empty -> (last_node, dist+1)
    | Border -> 
      begin
        let curr_id = match cell.id with
          | None -> failwith "Border should have one id"
          | Some (id, _) -> id in
        match last_node with
        | None -> (Some curr_id, 0)
        | Some other_id -> 
          graph.(other_id) <- 
            add_edge_to_node curr_id dist graph.(other_id);
          graph.(curr_id) <- 
            add_edge_to_node other_id dist graph.(curr_id);
          (None,0)
      end
    | _ -> begin
        match last_node with
        |None -> failwith "Mirrors should have a previous node"
        |Some other_id -> let curr_id, next_id = match cell.id with
            | None -> failwith "Border should have one id"
            | Some (id, x) -> match x with
              | None -> failwith "Mirrors should have 2 ids"
              | Some id2 -> (id,id2) in
          graph.(other_id) <- 
            add_edge_to_node curr_id dist graph.(other_id);
          graph.(curr_id) <- 
            add_edge_to_node other_id dist graph.(curr_id);
          (Some next_id,0) 
      end in
  let gen_v_node_cell = fun (last_node, dist) cell ->
    match cell.kind with
    | Empty -> (last_node, dist+1)
    | Border -> 
      begin
        let curr_id = match cell.id with
          | None -> failwith "Border should have one id"
          | Some (id, _) -> id in
        match last_node with
        | None -> (Some curr_id, 0)
        | Some other_id -> 
          graph.(other_id) <- 
            add_edge_to_node curr_id dist graph.(other_id);
          graph.(curr_id) <- 
            add_edge_to_node other_id dist graph.(curr_id);
          (None,0)
      end
    | _ -> begin
        match last_node with
        |None -> failwith "Mirrors should have a previous node"
        |Some other_id -> let curr_id, next_id = match cell.id with
            | None -> failwith "Border should have one id"
            | Some (id, x) -> match x with
              | None -> failwith "Mirrors should have 2 ids"
              | Some id2 -> match cell.kind with
                | MirrorA -> (id2,id)
                | MirrorS -> (id,id2)
                | _ -> failwith "impossible" in
          graph.(other_id) <- 
            add_edge_to_node curr_id dist graph.(other_id);
          graph.(curr_id) <- 
            add_edge_to_node other_id dist graph.(curr_id);
          (Some next_id,0) 
      end in
  let gen_h_node_line_iteri = fun i labo_line ->
    if i <> 0 && i <> (Array.length labo)-1 then
      let _ = Array.fold_left gen_h_node_cell_fold (None,0) labo_line in
      () in
  let gen_v_node_col = fun c ->
    let acc = ref (None, 0) in
    for l=0 to Array.length labo - 1 do
      acc := gen_v_node_cell !acc labo.(l).(c)
    done in
  Array.iteri gen_h_node_line_iteri labo;
  for c=1 to Array.length (labo.(0)) - 2 do
    gen_v_node_col c
  done;
  graph

let compute_max_laser = fun graph ->
  let visited = Array.make (Array.length graph) false in
  let compute_length = fun inode ->
    let rec compute_length_rec = fun inode length ->
      visited.(inode) <- true;
      match graph.(inode) with
      | None -> failwith "Shouldnt have isolated node"
      | Some (e1, None) -> if visited.(e1.id) then
          length
        else
          compute_length_rec e1.id e1.dist
      | Some (e1, Some e2) -> if visited.(e1.id) && visited.(e2.id) then
          length+1
        else if visited.(e1.id) then
          compute_length_rec e2.id (length+1+e2.dist)
        else if visited.(e2.id) then
          compute_length_rec e1.id (length+1+e1.dist)
        else
          begin
            compute_length_rec e1.id (e2.dist+1+e1.dist)
          end in
    compute_length_rec inode 0 in
  let compute_max_acyclic_fold = fun (max,inode) node ->
    if visited.(inode) then
      (max,inode+1)
    else
      match node with
      | None -> failwith "Shouldnt have isolated node"
      | Some (_, Some _) -> (max,inode+1)
      | Some (e1, None) -> let length = compute_length inode in
        if length > max then
          (length,inode+1)
        else
          (max,inode+1) in
  let compute_max_cyclic_fold = fun (max,inode) node ->
    if visited.(inode) then
      (max,inode+1)
    else
      match node with
      | None -> failwith "Shouldnt have isolated node"
      | Some (_, None) -> failwith "Shouldnt have unvisited border node"
      | Some (e1, Some e2) -> let length = compute_length inode in
        if length > max then
          (length,inode+1)
        else
          (max,inode+1) in
  let max_acyclic,_ = 
    Array.fold_left compute_max_acyclic_fold (0,0) graph in
  let max_cyclic,_ =
    Array.fold_left compute_max_cyclic_fold (0,0) graph in
  if max_acyclic > max_cyclic then
    max_acyclic
  else
    max_cyclic

let _ =
  let labo, nb_nodes = read_labo() in
  let graph = gen_graph labo nb_nodes in
  Printf.printf "%d\n" (compute_max_laser graph)
