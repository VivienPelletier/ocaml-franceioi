open Robot;;

for tour=1 to 108 do
    for i=1 to 13 do
        haut ();
    done;
    for i=1 to 13 do
        droite ();
    done;
    for i=1 to 13 do
        bas ();   
    done;   
    for i=1 to 13 do
        gauche ();
    done;
done;
