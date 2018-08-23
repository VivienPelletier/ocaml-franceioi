let _ = 
    let explode = fun s ->
        let rec explode2 = fun i acc ->
            if i < 0 then
                acc
            else
                explode2 (i-1) (s.[i]::acc)
        in
        explode2 ((String.length s)-1) []
    and compact = fun note acc ->
        match acc with
        | [] -> [note]
        | hd :: tl ->
                if hd = note then 
                    tl
                else
                    note :: acc
        in
        let music = explode (Scanf.scanf " %s" (fun x -> x)) in
        List.iter (fun x -> output_char stdout x) (List.fold_right compact music []);
        print_newline();
