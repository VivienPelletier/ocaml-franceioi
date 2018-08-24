let line = input_line stdin
let _ = print_endline (String.uppercase_ascii line)
    (* Le site ne connait pas String.uppercase_ascii donc j'ai utilisé String.uppercase.
     * Cependant, String.uppercase est deprecated donc je laisse la version avec String.uppercase_ascii *)
