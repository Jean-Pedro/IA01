;; Variables

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





;; Fonctions de service

(defun successeurs-valides (case carte chemin)
  (if (and (and (numberp case) (listp carte)) (listp chemin)) ; vérification des paramètres
      (let ((succ (cdr (assoc case carte)))) ; on ajoute tous les successeurs de case
        (remove-if #'(lambda (x) (member x chemin)) succ)) ; on retire ceux déjà dans chemin
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

; Fonction ajoutée car Voldemort peut se déplacer partout sauf sur la case où il est déjà (choix arbitraire)
(defun successeurs-voldemort (case carte)
  (if (and (numberp case) (listp carte)) ; vérification des paramètres
      (let ((succ (mapcar #'car carte))) ; on récupère toutes les cases sur lesquelles il est possible de se déplacer
        (remove-if (lambda (x) (eql case x)) succ)) ; on retire la case sur laquelle se trouve Voldemort
    "Erreur de type dans la saisie des paramètres"
    )
  )

(defun allHorcDetruits (listH listHrecup)
  (if (and (listp listH) (listp listHrecup)) ; vérification des paramètres
      (let ((listeHorc (mapcar #'cadr listH)) (test 0))
        (loop while (and (eq 0 test) (> (length listeHorc) 0)) do
              (if (member (car listeHorc) listHrecup :test #'string=)
                  (pop listeHorc)
                (setq test 1))
              )
        (if (eq test 0)
            T
          nil)
        )))

(defun backtracking (chemin cheminGeneral map)
  (if (and (and (listp chemin) (listp cheminGeneral)) (listp map)) ; vérification des paramètres
      (let ((chem (reverse chemin)))
        (pop chem)
        (loop while (and (not (successeurs-valides (car chem) map cheminGeneral)) chem) do
              (pop chem)
              )
        (if (not chem)
            nil
          (reverse chem)))
    "Erreur de type dans la saisie des paramètres"
    )
  )

; Tests des fonctions de service

(successeurs-valides '25 map '(1 12 13 24))

(methodeDestruction "Nagini" horcruxesDescription)

(hasBonneArme "Nagini" '("Crochet de Basilic" "Epée de Gryffondor") horcruxesDescription)

(successeurs-voldemort '4 map)

(backtracking '(1 12 13 24 25 26) '(1 12 13 24 25 36) map)

(allHorcDetruits horcruxesMap '("Nagini" "Diadème de Rowena Serdaigle"))


;; Recherche en profondeur pour Harry Potter avec variables globales

(defvar *armesCollectees* nil)
(defvar *horcruxesDetruits* nil)
(defvar *horcruxesIncassables* nil)


(defun recherche-en-profondeur-H (caseDepart carte horcruxesDescription armesMap horcruxesMap chemin)
  (if (and (and (and (and (and (numberp caseDepart) (listp carte)) (listp horcruxesDescription)) (listp armesMap)) (listp horcruxesMap)) (listp chemin)) ; vérification des paramètres
      (progn 
        (if (not (allHorcDetruits horcruxesMap *horcruxesDetruits*))
            (progn
              (format t "~%Harry Potter est actuellement sur la case ~a.~%" caseDepart)
              (let ((armeSurCase (assoc caseDepart armesMap)) (horcruxeSurCase (assoc caseDepart horcruxesMap)) (successeurs (successeurs-valides caseDepart carte chemin)))
                ; définition des variables qui nous seront utiles par la suite
                (if (not (member caseDepart chemin)) (setq chemin (append chemin (list caseDepart)))) ; on ajoute la case actuelle au chemin parcourru
                (if (and armeSurCase (not (member (cadr armeSurCase) *armesCollectees* :test #'string=))) ; test de s'il y a une arme que l'on ne possède pas
                    (progn
                      (push (cadr armeSurCase) *armesCollectees*) ; ajout de l'arme dans la liste des possédées
                      (format t "Harry Potter a trouvé ~a~%" (cadr armeSurCase))))
                (if (and (and horcruxeSurCase (not (member (cadr horcruxeSurCase) *horcruxesDetruits* :test #'string=))) (not (member (cadr horcruxeSurCase) *horcruxesIncassables* :test #'string=))) ; test de s'il y a un horcruxe encore destructible sur la case
                    (if (hasBonneArme (cadr horcruxeSurCase) *armesCollectees* horcruxesDescription) ; test de si on possède l'arme
                        (progn
                          (push (cadr horcruxeSurCase) *horcruxesDetruits*) ; si oui on le détruit
                          (format t "Harry Potter a détruit le horcruxe : ~a~%" (cadr horcruxeSurCase)))
                      (format t "Harry est tombé sur ~a, mais il ne possède pas ~a. Cet horcruxe n'est maintenant plus destructible~%" (cadr horcruxeSurCase) (methodeDestruction (cadr horcruxeSurCase) horcruxesDescription))
                      ))
                (if (not (allHorcDetruits horcruxesMap *horcruxesDetruits*))
                (if (or (>= (length chemin) 8) (null successeurs)) ; profondeur maximale atteinte ou plus de successeurs
                    (progn
                      (format t "~%Fin de la recherche de cette branche")
                      (format t "~%chemin parcouru : ~a~%~%" chemin))
                  (dolist (successeur successeurs) ; déplacement vers le successeur suivant
                    (format t "Harry Potter se déplace en case ~a~%" successeur)
                    (recherche-en-profondeur-H successeur carte horcruxesDescription armesMap horcruxesMap chemin)
                    )))
                ))
          ))))



(defun code-Harry (case) ; permet de faire la recherche et l'affichage en une seule instruction dans la console
  (setq *horcruxesDetruits* '()) ; permet de vider les variables globales dans le cas où on aurait déjà 
  (setq *armesCollectees* '())   ; effectué une recherche en profondeur
  (setq *horcruxesIncassables* '())
  (recherche-en-profondeur-H case map horcruxesDescription armesMap horcruxesMap '())
  (format t "~%Résultat final~%")
  (format t "~%Horcruxe(s) Détruit(s) : ~s ~%Méthode(s) Trouvée(s) : ~s" *horcruxesDetruits* *armesCollectees*)
  "Fin"
  )


; Test de la recherche en profondeur

(code-harry 1)


;; Lord Voldemort (avec variables globales)

(defvar *horcruxesDetruitsH* '())
(defvar *armesCollecteesH* '())
(defvar *horcruxesIncassables* '())
(defvar *horcruxesDetruitsV* '())
(defvar *armesCollecteesV* '())
(defvar *cheminV* '())
(defvar *result* nil)



(defun voldemort-glob (caseH caseV carte horcruxesDescription armesMap horcruxesMap cheminH)
  (if (and (and (and (and (and (and (numberp caseH) (numberp caseV)) (listp carte)) (listp horcruxesDescription)) (listp armesMap)) (listp horcruxesMap)) (listp cheminH))
      (if (and (not (member "Harry Potter" *horcruxesDetruitsV* :test #'string=)) (not (allHorcDetruits horcruxesMap (append *horcruxesDetruitsH* *horcruxesDetruitsV*))))
          (progn
            (format t "~%Harry Potter est actuellement sur la case ~a.~%" caseH)
            (let ((armeSurCaseH (assoc caseH armesMap)) (horcruxeSurCaseH (assoc caseH horcruxesMap)) (successeursH (successeurs-valides caseH carte cheminH))
                  (armeSurCaseV (assoc caseV armesMap)) (horcruxeSurCaseV (assoc caseV horcruxesMap)) (successeursV (successeurs-voldemort caseV carte))
                  )
              ; partie test des cases de Harry
              (if (not (member caseH cheminH)) (setq cheminH (append cheminH (list caseH))))
              (if (and (and armeSurCaseH (not (member (cadr armeSurCaseH) *armesCollecteesH* :test #'string=))) (not (member (cadr armeSurCaseH) *armesCollecteesV* :test #'string=)))
                  (progn
                    (push (cadr armeSurCaseH) *armesCollecteesH*)
                    (format t "Harry Potter a trouvé ~a~%" (cadr armeSurCaseH))))
              (if (and (and (and horcruxeSurCaseH (not (member (cadr horcruxeSurCaseH) *horcruxesDetruitsH* :test #'string=))) (not (member (cadr horcruxeSurCaseH) *horcruxesDetruitsV* :test #'string=))) (not (member (cadr horcruxeSurCaseH) *horcruxesIncassables* :test #'string=))) 
                  (if (hasBonneArme (cadr horcruxeSurCaseH) *armesCollecteesH* horcruxesDescription)
                      (progn
                        (push (cadr horcruxeSurCaseH) *horcruxesDetruitsH*)
                        (format t "Harry Potter a détruit le horcruxe : ~a~%" (cadr horcruxeSurCaseH)))
                    ))
              
              
              (if (< (length *cheminV*) (length cheminH))
                  (progn 
                    (setq *cheminV* (append *cheminV* (list caseV)))
                    (format t "~%Voldemort est actuellement sur la case ~a.~%" caseV)
                    
                    ; partie test des cases de Voldemort
                    
                    (if (and (and armeSurCaseV (not (member (cadr armeSurCaseV) *armesCollecteesV* :test #'string=))) (not (member (cadr armeSurCaseV) *armesCollecteesH* :test #'string=)))
                        (progn
                          (push (cadr armeSurCaseV) *armesCollecteesV*)
                          (format t "Voldemort a trouvé ~a~%" (cadr armeSurCaseV))))
                    (if (and (and (and horcruxeSurCaseV (not (member (cadr horcruxeSurCaseV) *horcruxesDetruitsV* :test #'string=))) (not (member (cadr horcruxeSurCaseV) *horcruxesDetruitsH* :test #'string=))) (not (member caseV (butlast *cheminV*)))) 
                        (if (hasBonneArme (cadr horcruxeSurCaseV) *armesCollecteesV* horcruxesDescription)
                            (progn
                              (push (cadr horcruxeSurCaseV) *horcruxesDetruitsV*)
                              (format t "Voldemort a détruit le horcruxe : ~a~%" (cadr horcruxeSurCaseV)))
                          ))
                    (if (not (allHorcDetruits horcruxesMap (append *horcruxesDetruitsH* *horcruxesDetruitsV*)))
                        (progn
                          (format t "Voici la liste des cases sur lesquelles il peut se rendre : ~a~%" successeursV)
                          (loop while (not (member *result* successeursV))
                              do (progn
                                   (format t "Veuillez choisir la case sur laquelle vous souhaitez déplacer Voldemort (dans la liste des successeurs)~%")
                                   (setq *result* (read)))))))
                (if (and (member "Sortilège de la Mort" *armesCollecteesV* :test #'string=)(eq caseH *result*))
                    (progn
                      (push "Harry Potter" *horcruxesDetruitsV*)
                      (setq *cheminV* (append *cheminV* (list *result*)))
                      ))
                )
              
              (if (and (member "Sortilège de la Mort" *armesCollecteesV* :test #'string=)(eq (car successeursH) *result*))
                  (progn
                    (push "Harry Potter" *horcruxesDetruitsV*)
                    (setq *cheminV* (append *cheminV* (list *result*)))
                    ))
              
              (if (not (allHorcDetruits horcruxesMap (append *horcruxesDetruitsH* *horcruxesDetruitsV*)))
                  (if (or (>= (length cheminH) 8) (null successeursH))
                      (progn
                        (format t "~%Fin de la recherche de cette branche")
                        (format t "~%chemin parcouru : ~a~%" cheminH))
                    (dolist (successeur successeursH)
                      (format t "~%Harry Potter se déplace en case ~a" successeur)
                      (voldemort-glob successeur *result* carte horcruxesDescription armesMap horcruxesMap cheminH)
                      )))
              ))
    
        ))
    )


(defun code-voldemort-glob (caseH caseV) ; permet de faire la recherche et l'affichage en une seule instruction dans la console
  (setq *cheminV* '())
  (setq *result* caseV)
  (setq *horcruxesDetruitsH* '()) ; permet de vider les variables globales dans le cas où on aurait déjà 
  (setq *armesCollecteesH* '())   ; effectué une recherche en profondeur
  (setq *horcruxesIncassables* '())
  (setq *horcruxesDetruitsV* '())
  (setq *armesCollecteesV* '())
  (voldemort-glob caseH caseV map horcruxesDescription armesMap horcruxesMap '())
  (format t "~%Résultat final~%")
  (format t "~%Harry : Horcruxe(s) Détruit(s) : ~s ~%Méthode(s) Trouvée(s) : ~s" *horcruxesDetruitsH* *armesCollecteesH*)
  (format t "~%Voldemort : Horcruxe(s) Détruit(s) : ~s ~%Méthode(s) Trouvée(s) : ~s" *horcruxesDetruitsV* *armesCollecteesV*)
  (format t "~%Chemin de Voldemort : ~s" *cheminV*)
  "FIN"
  )


(code-voldemort-glob 1 32)



(defun recherche-harry (caseDepart map horcruxesMap armesMap horcruxesDescription profondeur)
  (if (and (and (and (and (and (numberp caseDepart) (listp map)) (listp horcruxesDescription)) (listp armesMap)) (listp horcruxesMap))) ; vérification des paramètres
      (progn 
        (let ((case 'nil) 
              (chemin '())
              (cheminGeneral '())
              (horcDetruits '()) 
              (armesCollectees '()) 
              (successeurs '())
              (succ nil)
              (armeSurCase nil) 
              (prof 0)
              (horcruxeSurCase nil))
          (push caseDepart successeurs)
          (loop while (and (and successeurs (>= prof 0)) (not (allHorcDetruits horcruxesMap horcDetruits))) do
              (setq chemin (append chemin (list (car successeurs)))) 
                     
              (setq cheminGeneral (append cheminGeneral (list (car successeurs))))        
              (setq case (pop successeurs)) 
              (format t "Harry se trouve en case ~s.~%" case)
              (setq prof (+ prof 1))
              
              (setq armeSurCase (cadr (assoc case armesMap)))
              (setq horcruxeSurCase (cadr (assoc case horcruxesMap)))
              (if (and armeSurCase (not (member armeSurCase armesCollectees :test #'string=)))
                  (progn
                    (push armeSurCase armesCollectees)
                    (format t "Harry Potter a trouvé ~a~%" armeSurCase)))
              
              (if (and horcruxeSurCase (not (member case (butlast cheminGeneral))))
                  (if (hasBonneArme horcruxeSurCase armesCollectees horcruxesDescription)
                      (progn
                        (push horcruxeSurCase horcDetruits)
                        (format t "Harry Potter a détruit le horcruxe : ~a~%" horcruxeSurCase))))

              
              (setq succ (successeurs-valides case map cheminGeneral))
              (if (and (< prof profondeur) succ)                       
                  (progn
                    (dolist (x (reverse succ))
                      (push x successeurs)) 
                    (format t "~%"))
                (progn
                  (format t "Parcours de la branche terminée, chemin parcouru : ~a~%~%" chemin)
                  (setq chemin (backtracking chemin cheminGeneral map))
                  (setq prof (- (length chemin) 1))
                  )))
        
          (values armesCollectees horcDetruits))
        )
    )
  )


(recherche-harry 1 map horcruxesMap armesMap horcruxesDescription 7)



(defun voldemort (caseDepart caseV map horcruxesMap armesMap horcruxesDescription profondeur)
  (if (and (and (and (and (and (numberp caseDepart) (listp map)) (listp horcruxesDescription)) (listp armesMap)) (listp horcruxesMap))) ; vérification des paramètres
      (progn 
        (let ((caseH 'nil)
              (cheminH '())
              (cheminGeneralH '())
              (cheminV '())
              (horcDetruitsH '()) 
              (armesCollecteesH '()) 
              (horcDetruitsV '()) 
              (armesCollecteesV '()) 
              (successeurs '())
              (succ nil)
              (armeSurCaseH nil) 
              (horcruxeSurCaseH nil)
              (armeSurCaseV nil) 
              (horcruxeSurCaseV nil)
              (profH 0)
              (prof-maxH 0)
              (succV nil)
              (result nil))
          
          (push caseDepart successeurs)
          (loop while (and (and (and successeurs (>= profH 0)) (not (member "Harry Potter" horcDetruitsV :test #'string=))) (not (allHorcDetruits horcruxesMap (append horcDetruitsH horcDetruitsV)))) do
                      (setq cheminH (append cheminH (list (car successeurs))))                     
                      (setq cheminGeneralH (append cheminGeneralH (list (car successeurs))))        
                      (setq caseH (pop successeurs)) 
                      (format t "Harry se trouve en case ~s.~%" caseH)
                      (setq profH (+ profH 1))
                      (if (< prof-maxH profH) (setq prof-maxH (+ prof-maxH 1)))
                      (setq armeSurCaseH (cadr (assoc caseH armesMap)))
                      (setq horcruxeSurCaseH (cadr (assoc caseH horcruxesMap)))
                      (setq succ (successeurs-valides caseH map cheminGeneralH))
                
                      ; partie Harry 
                      (if (and (and armeSurCaseH (not (member armeSurCaseH armesCollecteesH :test #'string=))) (not (member armeSurCaseH armesCollecteesV : test #'string=)))
                          (progn
                            (push armeSurCaseH armesCollecteesH)
                            (format t "Harry Potter a trouvé ~a~%" armeSurCaseH)))
              
                      (if (and (and horcruxeSurCaseH (not (member caseH (butlast cheminGeneralH)))) (not (member armeSurCaseH armesCollecteesV :test #'string=)))
                          (if (hasBonneArme horcruxeSurCaseH armesCollecteesH horcruxesDescription)
                              (progn
                                (push horcruxeSurCaseH horcDetruitsH)
                                (format t "Harry Potter a détruit le horcruxe : ~a~%" horcruxeSurCaseH))))

                      ; partie Voldemort
                      (if (< (length cheminV) prof-maxH)
                          (progn
                            (setq cheminV (append cheminV (list caseV))) 
                            (setq armeSurCaseV (cadr (assoc caseV armesMap)))
                            (setq horcruxeSurCaseV (cadr (assoc caseV horcruxesMap)))
                            (format t "~%Voldemort est actuellement sur la case ~a.~%" caseV)
                            
                            (if (and (and armeSurCaseV (not (member armeSurCaseV armesCollecteesV :test #'string=))) (not (member armeSurCaseV armesCollecteesH :test #'string=)))
                                (progn
                                  (push armeSurCaseV armesCollecteesV)
                                  (format t "Voldemort a trouvé ~a~%" armeSurCaseV)))
                            
                            (if (and (and (and horcruxeSurCaseV (not (member horcruxeSurCaseV horcDetruitsV :test #'string=))) (not (member horcruxeSurCaseV horcDetruitsH :test #'string=))) (not (member caseV (butlast cheminV)))) 
                                (if (hasBonneArme horcruxeSurCaseV armesCollecteesV horcruxesDescription)
                                    (progn
                                      (push horcruxeSurCaseV horcDetruitsV)
                                      (format t "Voldemort a détruit le horcruxe : ~a~%" horcruxeSurCaseV))
                                  ))
                            
                            (if (allHorcDetruits horcruxesMap (append HorcDetruitsH horcDetruitsV))
                                (return))
                      
                            (setq succV (successeurs-voldemort caseV map))
                            (format t "Voici la liste des cases sur lesquelles il peut se rendre : ~a~%" succV)
                            (loop while (not (member result succV))
                                do (progn
                                     (format t "Veuillez choisir la case sur laquelle vous souhaitez déplacer Voldemort (dans la liste des successeurs)~%")
                                     (setq result (read))
                                     (setq caseV result)
                                     )))
                        
                        (if (and (member "Sortilège de la Mort" armesCollecteesV :test #'string=)(eq caseH result))
                            (progn
                              (push "Harry Potter" horcDetruitsV)
                              (setq cheminV (append cheminV (list result)))
                              )))
                      
                      (if (and (member "Sortilège de la Mort" armesCollecteesV :test #'string=)(eq (car succ) result))
                          (progn
                            (push "Harry Potter" horcDetruitsV)
                            (setq cheminV (append cheminV (list result)))
                            ))
               
                      (if (and (< profH profondeur) succ)                       
                          (progn
                            (dolist (x (reverse succ))
                              (push x successeurs)) 
                            (format t "Harry se déplace en case ~s~%~%" (car successeurs)))
                        (progn
                          (format t "Parcours de la branche terminée, chemin parcouru : ~a~%" cheminH)
                          (setq cheminH (backtracking cheminH cheminGeneralH map))
                          (setq profH (- (length cheminH) 1))
                          (format t "Harry se déplace en case ~s~%~%" (car successeurs))
                          )))
        
                  (values armesCollecteesH horcDetruitsH armesCollecteesV horcDetruitsV))
                )
          )
        )


(voldemort 1 32 map horcruxesMap armesMap horcruxesDescription 7)





