let char_of_int = fun i -> Char.chr (Char.code 'a' + i) in
let nbLettres = Scanf.scanf " %d" (fun x -> x) in
assert (nbLettres > 0);
assert (nbLettres <= 26);
let taille = 2*nbLettres-1 in
let char_of_line = fun l ->
    if l < nbLettres then
        char_of_int l
    else
        char_of_int (taille - 1 - l) in
let good_char = fun c l ->
    if c < l then
        output_char stdout (char_of_int c)
    else if c < (taille - l) then
        output_char stdout (char_of_line l)
    else
        output_char stdout (char_of_int (taille - c - 1)) in
for l=0 to taille/2 do
    for c=0 to taille-1 do
        good_char c l
    done;
    print_newline();
done;
for l=0 to taille/2-1 do
    let l2 = taille/2-1 - l in
    for c=0 to taille-1 do
        good_char c l2
    done;
    print_newline();
done;
