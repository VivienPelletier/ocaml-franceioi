let nbMesures = Scanf.scanf " %d" (fun x -> x)
and threshold = Scanf.scanf " %f" (fun x -> x) in
let mesures = Array.make nbMesures 0. in
for i=0 to nbMesures-1 do
    mesures.(i) <- Scanf.scanf " %f" (fun x -> x)
done;
let l = Array.to_list mesures in

let validate = fun l ->
    let validate_once = fun (prev,acc) curr -> (curr, acc && (abs_float (prev -. curr) < threshold)) in
    let (_, validation) = 
        match l with
        | [] -> (0., true)
        | hd::tl -> List.fold_left validate_once (hd,true) tl in
    validation
and filter = fun l ->
    let filter_once = fun (prev, prev2, acc) curr ->
        (curr, prev, ((prev2 +. curr)/. 2.)::acc) in
    match l with
    | [] -> []
    | hd::[] -> l
    | prev2::prev::tl -> let (last, _, acc) = List.fold_left filter_once (prev, prev2, [prev2]) tl in
    last::acc in

let rec multiple_filter = fun l acc ->
    if validate l then
        acc
    else
        multiple_filter (filter l) (acc+1) in
print_int (multiple_filter l 0);
