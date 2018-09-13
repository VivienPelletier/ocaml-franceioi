type dir_t = Top | Bot | Left | Right
type kind_t = Empty | MirrorS | MirrorA
type cell_t = {
  kind : kind_t;
  (* sens 1 : Empty -> horizontal | Mirror -> left side *)
  mutable visited1 : bool;
  (* sens 2 : Empty -> vertical | Mirror -> right side *)
  mutable visited2 : bool;
}

let read_labo = fun () ->
  let nb_lines, nb_cols = 
    let line = read_line() in
    Scanf.sscanf line " %d %d" (fun l c -> l,c) in
  let cell_of_char = fun c ->
    {kind = begin
        match c with
        | '.' -> Empty
        | '/' -> MirrorS
        | '\\' -> MirrorA
        | _ -> failwith "bad char in input"
      end;
     visited1=false;
     visited2=false;
    } in
  let init_line = fun i ->
    let line = read_line() in
    Array.init nb_cols (fun i -> cell_of_char line.[i]) in
  Array.init nb_lines init_line

let compute_max_length = fun labo ->
  assert (Array.length labo > 0);
  let max_length = ref 0 in
  let get_sens = fun dir kind ->
    match (dir,kind) with 
    | (Left, Empty) | (Right, Empty)
    | (Bot,MirrorS) | (Top, MirrorA)
    | (Right, _) -> 1
    | (Top, Empty) | (Bot, Empty) 
    | (Bot, MirrorA) | (Top, MirrorS)
    | (Left, _) -> 2 in
  let compute_length = fun l c sens ->
    let rec compute_length_rec = fun l c dir length ->
      (* termination case : border *)
      if l < 0 || c < 0 ||
         l >= Array.length labo || c >= Array.length labo.(0) then
        length
      else
        let cell = labo.(l).(c) in
        let sens = get_sens dir cell.kind in
        (* termination case : cycle *)
        if (sens = 1 && cell.visited1) || (sens = 2 && cell.visited2) then
          length
        else
          begin
            (* Set as visited *)
            begin
              if sens = 1 then
                cell.visited1 <- true
              else
                cell.visited2 <- true
            end;
            match cell.kind with
            | Empty -> begin
                match dir with
                | Top -> compute_length_rec (l-1) c dir (length+1)
                | Bot -> compute_length_rec (l+1) c dir (length+1)
                | Left -> compute_length_rec l (c-1) dir (length+1)
                | Right -> compute_length_rec l (c+1) dir (length+1)
              end
            | MirrorA -> begin
                match dir with
                | Top -> compute_length_rec l (c-1) Left (length+1)
                | Bot -> compute_length_rec l (c+1) Right (length+1)
                | Left -> compute_length_rec (l-1) c Top (length+1)
                | Right -> compute_length_rec (l+1) c Bot (length+1)
              end
            | MirrorS -> begin 
                match dir with
                | Top -> compute_length_rec l (c+1) Right (length+1)
                | Bot -> compute_length_rec l (c-1) Left (length+1)
                | Left -> compute_length_rec (l+1) c Bot (length+1)
                | Right -> compute_length_rec (l-1) c Top (length+1)
              end
          end
    in
    match labo.(l).(c).kind with
    | Empty -> if sens=1 then
        compute_length_rec l c Right 0
      else
        compute_length_rec l c Bot 0
    | MirrorS -> if sens=1 then
        compute_length_rec l c Right 0
        + compute_length_rec l (c-1) Left 0
      else
        compute_length_rec l c Left 0
        + compute_length_rec l (c+1) Right 0
    | MirrorA -> if sens=1 then
        compute_length_rec l c Right 0
        + compute_length_rec l (c-1) Left 0
      else
        compute_length_rec l c Left 0
        + compute_length_rec l (c+1) Right 0 in
  let visit_cell = fun l c cell ->
    begin
      if not cell.visited1 then
        let length = compute_length l c 1 in
        if length > !max_length then
          max_length := length
    end;
    begin
      if not cell.visited2 then
        let length = compute_length l c 2 in
        if length > !max_length then
          max_length := length
    end in
  Array.iteri (fun l line ->
      Array.iteri (fun c cell -> visit_cell l c cell) line) labo;
  !max_length

let _ =
  let labo = read_labo() in
  Printf.printf "%d\n" (compute_max_length labo);
