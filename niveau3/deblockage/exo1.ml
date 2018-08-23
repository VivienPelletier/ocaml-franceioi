(* fonction pour lire une pair d'entiers sur une ligne *)
let lire_pair() = Scanf.scanf " %d %d" (fun x y -> x, y)
and lire_entier() = Scanf.scanf " %d" (fun x -> x) in
(* récupération du nombre de livre et du nobmre de jours *)
let nbLivres, nbJours = lire_pair() in
    if nbLivres > 1000 then
        print_endline "Erreur : nb livre trop grand"
    else
        let livres = Array.make nbLivres 0 in
        (* pour chaque jour *)
        for iJour=1 to nbJours do
            let nbClients = lire_entier() in
            (* pour chaque client *)
            for iClient=1 to nbClients do
                let iLivre, duree = lire_pair() in
                (* si le livre est disponible *)
                if livres.(iLivre) <= iJour then
                    begin
                    livres.(iLivre) <- iJour + duree;
                    print_endline "1";
                    end
                else
                    print_endline "0"
            done;(* fin client *)
        done;(* fin jour *)
