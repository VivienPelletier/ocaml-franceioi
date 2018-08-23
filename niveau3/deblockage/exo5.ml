let scan_pair = fun () -> 
    Scanf.scanf " %d %d" (fun x y -> x, y)
let scan_int = fun () ->
    Scanf.scanf " %d" (fun x -> x)

let nbFrogs = scan_int()
let nbTurns = scan_int()

let positions = Array.make nbFrogs 0
let scores = Array.make nbFrogs 0

let winner_once = fun (max, imax, draw, icurr) curr ->
    if curr < max then
        (max, imax, draw, icurr+1)
    else if max = curr then
        (max, imax, true, icurr+1)
    else
        (curr, icurr, false, icurr+1)

let _ =
    for i=1 to nbTurns-1 do
        let frog, distance = scan_pair() in 
        positions.(frog-1) <- positions.(frog-1)+distance;
        let (_, iwinner, draw, _) = Array.fold_left winner_once (-1,-1,false,0) positions in
        if not draw then
            scores.(iwinner) <- scores.(iwinner)+1;
    done;
    let (_, iwinner, draw, _) = Array.fold_left winner_once (-1,-1,false,0) scores in
    print_int (iwinner+1);
    print_newline ();
