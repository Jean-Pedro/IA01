Le code fourni est le TP 2 de IA01 réalisé en A23. Tout le code est contenu dans le même fichier. Il contient les deux versions de la recherche en profondeur de Harry et du code de Voldemort.



;; Listes : 

(setq map '((1 12 2)(2 1 3)(3 2 4)(4 3 5)(5 4 8 6)(6 5 7)(7 8 6)(8 7 5)(12 13 1)
            (13 24 12)(15 22)(20 21 29)(21 22 20)(22 27 21 15)(24 25 13)
            (25 36 26 24)(26 25 27)(27 26 22)(29 32 20)(32 29)(36 25)))


(setq horcruxesDescription '(("Journal intime de Tom Jedusor" 
                                (methodeDestruction "Crochet de Basilic"))
                             ("Médaillon de Salazar Serpentard" 
                                (methodeDestruction "Epée de Gryffondor"))
                             ("Bague de Gaunt" 
                                (methodeDestruction "Epée de Gryffondor"))
                             ("Coupe de Helga Poufsouffle" 
                                (methodeDestruction "Crochet de Basilic"))
                             ("Nagini" 
                                (methodeDestruction "Epée de Gryffondor"))
                             ("Diadème de Rowena Serdaigle" 
                                (methodeDestruction "Feudeymon"))))

(setq horcruxesMap '((8 "Journal intime de Tom Jedusor")
                     (12 "Médaillon de Salazar Serpentard")
                     (15 "Bague de Gaunt")
                     (22 "Coupe de Helga Poufsouffle")
                     (26 "Nagini")
                     (29 "Diadème de Rowena Serdaigle")))

(setq armesMap '((3 "Crochet de Basilic")
                 (32 "Feudeymon")
                 (25 "Epée de Gryffondor")
                 (20 "Sortilège de la Mort"))) 

;; Tests
;; Fonctions de service

(successeurs-valides '25 map '(1 12 13 24))
	Cette fonction prend en paramètres la case actuelle, la carte du labyrinthe et le chemin déjà parcouru. Elle renvoie une liste contenant tous les successeurs qui n'ont pas déjà été parcourus.

(methodeDestruction "Nagini" horcruxesDescription)
	Cette fonction prend en paramètres le nom du horcruxe et la liste des description des horcruxes et renvoie la méthode permettant de le détruire.  

(hasBonneArme "Nagini" '("Crochet de Basilic" "Epée de Gryffondor") horcruxesDescription)
	Cette fonction prend en paramètres le nom d'un horcruxe, une liste d'armes et la liste des descriptions des horcruxes. Elle va renvoyer T si il y a dans la liste des armes celle nécessaire pour détruire le horcruxe.

(successeurs-voldemort '4 map)
	Cette fonction prend en paramètres la case actuelle de voldemort et la carte du labyrinthe. Elle va renvoyer toutes les cases du labyrinthe excepté la case actuelle (ce qui correspond aux successeurs de Voldemort). 

(backtracking '(1 12 13 24 25 26) '(1 12 13 24 25 36) map)
	Cette fonction va prendre en paramètres le chemin actuel, le chemin général et la carte du labyrinthe. Elle va renvoyer le chemin précédent le plus proche qui contient un successeur non exploré. Par exemple ici la fonction va renvoyer (1 12 13 24 25) car 25 a pour successeur 26 qui n'a pas été exploré.

(allHorcDetruits horcruxesMap '("Nagini" "Diadème de Rowena Serdaigle"))
	Cette fonction va prendre en paramètres la carte des horcruxes et une liste de horcruxes (qui correspond à la liste des horcruxes détruits) et va renvoyer T si tous les horcruxes de la première liste sont présents dans la deuxième (c'est à dire si tous les horcruxes ont été détruits.


;; Codes avec variables globales
;;Harry
(code-harry 1)
	Cette fonction va prendre en paramètres la case de départ de Harry et va effectuer la recherche en profondeur.

;; Harry et Voldemort
(code-voldemort-glob 1 32)
	Cette fonction va prendre en paramètres la case de départ de Harry et de Voldemort et va effectuer la recherche en profondeur couplée aux déplacements de Voldemort.


Par manque de temps nous n'avons pas pu faire de fonction pour faire un affichage plus détaillé comme pour les deux fonction précédentes.

;; Codes sans variables globales
;; Harry
(recherche-harry 1 map horcruxesMap armesMap horcruxesDescription 7)
	Cette fonction prend en paramètres la case de harry, toutes les listes nécessaires et la profondeur maximale de la recherche.

;; Harry et Voldemort
(voldemort 1 32 map horcruxesMap armesMap horcruxesDescription 7)
	Cette fonction prend en paramètres la case de départ de Harry, celle de Voldemort, les listes nécessaires et la profondeur maximale de la recherche.


