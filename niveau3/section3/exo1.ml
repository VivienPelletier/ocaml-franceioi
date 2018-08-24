let nom = Scanf.scanf " %s" (fun x -> x)
let age = Scanf.scanf " %d" (fun x -> x)

let _ = Printf.printf "%d%c\n"
   ((Char.code (nom.[0]))-(Char.code 'A')+1)
   (Char.chr (age+(Char.code 'A'-1)));
