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



(defun successeurs-valides (case carte chemin)
  (if (and (and (numberp case) (listp carte)) (listp chemin)) ; vérification des paramètres
      (let ((succ (cdr (assoc case carte)))) ; on ajoute tous les successeurs de case
        (remove-if #'(lambda (x) (member x chemin)) succ)) ; on retire ceux déjà dans chemin
    "Erreur de type dans la saisie des paramètres"
    )
  )

(defun successeurs (case carte)
  (if (and(numberp case) (listp carte)) ; vérification des paramètres
      (cdr (assoc case carte)) ; on ajoute tous les successeurs de case
    "Erreur de type dans la saisie des paramètres"
    )
  )


(defun methodeDestruction (horc horcDesc)
  (if (and (stringp horc)(listp horcDesc)) ; vérification des paramètres
      (let ((desc (cadr (assoc horc horcDesc :test #'string=))))
        (if desc
            (cadr desc))
        )
    "Erreur de type dans la saisie des paramètres"
    ))



(defun hasBonneArme (horc listeM horcDesc)
  (if (and (and (stringp horc)(listp listeM))(listp horcDesc)) ; vérification des paramètres
      (let ((methode (methodeDestruction horc horcDesc))) ; on récupère la méthode de destruction de horc
        (if (member methode listeM :test #'string=) ; on teste si cette méthode est dans la liste des méthodes possédées
            T))
    "Erreur de type dans la saisie des paramètres"
    )
  )





(defun recherche-harry (caseDepart map horcruxesMap armesMap horcruxesDescription profondeur)
  (if (and (and (and (and (and (and (numberp caseDepart) (listp map)) (listp horcruxesDescription)) (listp armesMap)) (listp horcruxesMap))) (numberp profondeur)) ; vérification des paramètres
        (let ((chemin '())
              (cheminGeneral '())
              (horcDetruits '()) 
              (armesCollectees '()) 
              (succ (list caseDepart)) 
              (armeSurCase nil) 
              (prof 0)
              (prof-max 0)
              (horcruxeSurCase nil))
          (loop while (and (<= prof-max profondeur) succ)
              do (progn
                   (format t "Harry se trouve sur la case ~s.~%" caseDepart)
                   (setq armeSurCase (cadr (assoc caseDepart armesMap)))
                   (setq horcruxeSurCase (cadr (assoc caseDepart horcruxesMap)))
                   (setq chemin (append chemin (list caseDepart)))
                   (setq succ (successeurs-valides caseDepart map cheminGeneral))

                   (if (and armeSurCase (not (member armeSurCase armesCollectees :test #'string=))) ; test de s'il y a une arme que l'on ne possède pas
                       (progn
                         (push armeSurCase armesCollectees) ; ajoute de l'arme dans la liste des possédées
                         (format t "Harry Potter a trouvé ~a~%" armeSurCase)))

                   (if (and horcruxeSurCase (not (member caseDepart cheminGeneral)))
                       (if (hasBonneArme horcruxeSurCase armesCollectees horcruxesDescription)
                           (progn
                             (push horcruxeSurCase horcDetruits)
                             (format t "Harry Potter a détruit le horcruxe : ~a~%" horcruxeSurCase))
                         ))
                   
                   (setq cheminGeneral (append cheminGeneral (list caseDepart)))
                   
                   
                   (setq caseDepart (car succ))
                   (if (and (or (not succ) (= prof profondeur)) (backtracking map chemin cheminGeneral prof))
                       (let ((result (backtracking map chemin cheminGeneral prof)))
                         (setq caseDepart (first result))
                         (setq prof (second result))
                         (setq chemin (car (last result)))
                         (setq succ 'v))
                     (progn
                       (setq prof (+ prof 1))
                       (if (> prof prof-max)
                           (setq prof-max (+ prof-max 1)))
                       )
                     )
                   ))
          (values armesCollectees horcDetruits))))





(recherche-harry 1 map horcruxesMap armesMap horcruxesDescription 7)



(defun backtracking (map chemin cheminGeneral prof)
  (if (and (and (and (listp map) (listp chemin)) (listp cheminGeneral)) (numberp prof))
      (let ((newChemin (reverse chemin)) (result nil))
        (pop newChemin)
        (loop while (and newChemin (not (successeurs-valides (car newChemin) map cheminGeneral))) do
              (progn 
                (pop newChemin)
                (setq prof (- prof 1))))
        
        (if (not newChemin) 
            nil 
          (setq result (list (car (successeurs-valides (car newChemin) map cheminGeneral)) prof (reverse newChemin))))
        result
        )
    "Erreur dans la saisie des paramètres"))
          
(backtrack2 map '(1 12 13 24 25 36) '(1 12 13 24 25 36)  7)
