;; Définition de la base de règles
(defvar *BDF* nil)
(defvar *BDR* nil)
(setq *BDR*
      '(
        (R1 ((type0 eq ART)) (type1 eq ASSO_ART))
        (R2 ((type0 eq CULTURE)) (type1 eq ASSO_CULTURE))
        (R3 ((type0 eq GESTION)) (type1 eq ASSO_GESTION))
        (R4 ((type1 eq ASSO_ART) (activite eq THEATRE)) (type2 eq ASSO_THEATRE))
        (R5 ((type1 eq ASSO_ART) (activite eq MUSIQUE)) (type2 eq ASSO_MUSIC))
        (R6 ((type1 eq ASSO_ART) (activite eq MEDIA)) (type2 eq ASSO_MEDIA))
        (R7 ((type2 eq ASSO_THEATRE)) (asso eq PROFITROLES))
        (R8 ((type2 eq ASSO_MUSIC) (nb <= 2)) (asso eq FSC))
        (R9 ((type2 eq ASSO_MUSIC) (nb >= 3) (nb <= 6)) (asso eq LARSEN))
        (R10 ((type2 eq ASSO_MUSIC) (nb >= 7)) (type3 eq ASSO_MUSIC_G))
        (R11 ((type3 eq ASSO_MUSIC_G) (mode eq ORCHESTRE)) (type4 eq ASSO_ORCH))
        (R12 ((type3 eq ASSO_MUSIC_G) (mode eq FANFARE)) (type4 eq ASSO_FANF))
        (R13 ((type4 eq ASSO_ORCH) (genre eq JAZZ)) (asso eq OCATA))
        (R14 ((type4 eq ASSO_ORCH) (genre eq AUTRE_GENRE)) (asso eq STRAVAGANZA))
        (R15 ((type4 eq ASSO_FANF)) (asso eq CAPHARNAUM))
        (R16 ((type2 eq ASSO_MEDIA) (activite eq ECRITURE)) (type3 eq ASSO_ECRITURE))
        (R17 ((type2 eq ASSO_MEDIA) (activite eq DESSIN)) (type3 eq ASSO_DESSIN))
        (R18 ((type2 eq ASSO_MEDIA) (activite eq RADIO)) (ASSO eq GRAPHIT))
        (R19 ((type2 eq ASSO_MEDIA) (activite eq PHOTO-VIDEO)) (type3 eq ASSO_PHOTO))
        (R20 ((type2 eq ASSO_MEDIA) (activite eq VIDEOLUDIQUE)) (ASSO eq ARCADIA))
        (R21 ((type2 eq ASSO_MEDIA) (activite eq AUTRE)) (ASSO eq PIKECOUD))
        (R22 ((type3 eq ASSO_ECRITURE) (genre eq INFORMATIF)) (ASSO eq LE_FIL))
        (R23 ((type3 eq ASSO_ECRITURE) (genre eq ARTISTIQUE)) (ASSO eq LE_CID))
        (R24 ((type3 eq ASSO_DESSIN) (genre eq DESIGN)) (ASSO eq AZERO))
        (R25 ((type3 eq ASSO_DESSIN) (genre eq TOUT)) (ASSO eq DADA))
        (R26 ((type3 eq ASSO_PHOTO) (genre eq OUI)) (ASSO eq ETUSEXY))
        (R27 ((type3 eq ASSO_PHOTO) (genre eq NON)) (ASSO eq CINEMUT))
        (R28 ((type1 eq ASSO_CULTURE) (culture eq ETRANGERE)) (type2 eq ASSO_CULTURE_ETRANGERE))
        (R29 ((type1 eq ASSO_CULTURE) (culture eq FRANCAISE)) (type2 eq ASSO_CULTURE_FRANCAISE))
        (R30 ((type1 eq ASSO_CULTURE) (culture eq POP)) (type2 eq ASSO_CULTURE_POP))
        (R31 ((type2 eq ASSO_CULTURE_ETRANGERE)) (asso eq JAPONUTC))
        (R32 ((type2 eq ASSO_CULTURE_FRANCAISE)) (asso eq DEVOIR_DE_MEMOIRE))
        (R33 ((type2 eq ASSO_CULTURE_POP) (activite eq PHYSIQUE)) (type3 eq ASSO_POP_PHY))
        (R34 ((type2 eq ASSO_CULTURE_POP) (activite eq VIDEOLUDIQUE)) (asso eq LANUTC))
        (R35 ((type2 eq ASSO_CULTURE_POP) (activite eq AUTRE)) (asso eq UTESCAPE))
        (R36 ((type3 eq ASSO_POP_PHY) (materiel eq JEUX)) (asso eq LE_COIN_DU_JOUEUR))
        (R38 ((type3 eq ASSO_POP_PHY) (materiel eq LIVRES)) (asso eq BDBDEC))
        (R39 ((type1 eq ASSO_GESTION) (pole eq VIE_CAMPUS)) (asso eq PVDC))
        (R40 ((type1 eq ASSO_GESTION) (pole eq ART_EVENT)) (asso eq PAE))
        (R41 ((type1 eq ASSO_GESTION) (pole eq SOLIDARITE)) (asso eq PSEC))
        (R42 ((type1 eq ASSO_GESTION) (pole eq TECH_ENTREPRENEURIAT)) (asso eq PTE))
        (R43 ((type1 eq ASSO_GESTION) (pole eq INFORMATIQUE)) (asso eq SIMDE))
        (R44 ((type1 eq ASSO_GESTION) (pole eq GENERAL)) (asso eq BDE))
        )
      )

;; Définition des questions
(progn
  (setq Q1 '(TEST "Veuillez sélectionner le thème qui vous intéresse le plus parmis ces choix : ART, CULTURE et GESTION" (ART CULTURE GESTION)))
  (setq Q2 '(ASSO_ART "Quel genre d'art préférez vous ? THEATRE, MUSIQUE ou MEDIA ?" (THEATRE MUSIQUE MEDIA)))
  (setq Q3 '(ASSO_MUSIC "A combien souhaitez-vous que se fassent les représentations ? (entre 1 et 10)" (1 2 3 4 5 6 7 8 9 10)))
  (setq Q4 '(ASSO_MUSIC_G "Que préférez-vous entre un ORCHESTRE et une FANFARE ?" (ORCHESTRE FANFARE)))
  (setq Q5 '(ASSO_ORCH "Souhaitez-vous jouer du JAZZ ou un AUTRE_GENRE de musique ?" (JAZZ AUTRE_GENRE)))
  (setq Q6 '(ASSO_MEDIA "Quel type de média préférez-vous ? ECRITURE, DESSIN, RADIO, PHOTO-VIDEO, VIDEOLUDIQUE ou AUTRE ?" (ECRITURE DESSIN RADIO PHOTO-VIDEO VIDEOLUDIQUE AUTRE)))
  (setq Q7 '(ASSO_ECRITURE "Préférez-vous que ce média soit INFORMATIF ou seulement ARTISTIQUE ?" (INFORMATIF ARTISTIQUE)))
  (setq Q8 '(ASSO_DESSIN "Préférez-vous une association de DESIGN ou de TOUT type ?" (DESIGN TOUT)))
  (setq Q9 '(ASSO_PHOTO "Etes-vous à l'aise avec le fait que cette association implique de la nudité ? (OUI ou NON)" (OUI NON)))
  (setq Q10 '(ASSO_CULTURE "Quel genre de culture vous intéresse le plus ? Culture ETRANGERE, FRANCAISE ou POP ?" (ETRANGERE FRANCAISE POP)))
  (setq Q11 '(ASSO_CULTURE_POP "Préférez-vous que cette association soit basée sur un support PHYSIQUE, VIDEOLUDIQUE ou bien un AUTRE type ?" (PHYSIQUE VIDEOLUDIQUE AUTRE)))
  (setq Q12 '(ASSO_POP_PHY "Quel type de support physique ? Des JEUX de société ou bien des LIVRES ?" (JEUX LIVRES)))
  (setq Q13 '(ASSO_GESTION "Dans quel pole ? VIE_CAMPUS, ART_EVENT, SOLIDARITE, TECH_ENTREPRENEURIAT, INFORMATIQUE ou GENERAL ?" (VIE_CAMPUS SOLIDARITE TECH_ENTREPRENEURIAT INFORMATIQUE GENERAL)))
  )


(setq *BDQ* '(Q1 Q2 Q3 Q4 Q5 Q6 Q7 Q8 Q9 Q10 Q11 Q12 Q13))


(defun cclRegle (regle) (caddr regle))

(defun premisseRegle (regle) (cadr regle))

(defun numRegle (regle) (car regle))

; Fonction qui permet de lister les associations disponibles dans le SE
(defun liste_associations ()
  (let ((conclusions nil) (test nil))
    (dolist (regle *bdr*)
      (setq test (cclRegle regle))
      (if (eq (car test) 'asso)
          (push (caddr test) conclusions)))
    conclusions))
    
;(liste_associations)



; Fonction qui permet de comparer deux listes et tester leur égalité
(defun comparer-listes (liste1 liste2)
  (and (= (length liste1) (length liste2))
       (every #'equal liste1 liste2)))

; (find '(TYPE1 EQ ASSO_ART) '((TYPE1 EQ ASSO_ART) (type2 eq ASSO_MUSIC) (chouette)) :test #'comparer-listes)


; Fonction qui permet de vérifier la validification d'une règle
(defun verifPremisse (regle)
  (let ((test 0) (val nil) (val1 nil))
    (let ((conditions (premisseRegle regle)) (conclusion (cclRegle regle)))
      (while (and (eq 0 test) conditions)
        (setq val (car (last (assoc (caar conditions) *BDF*))))
        (setq val1 (car (last (car conditions))))
        (if (and val val1 (funcall (cadar conditions) val val1)) ; vérification de la validité de la premisse
            (setq conditions (cdr conditions))
          (progn
            (setq test 1) ; on sort de la boucle dès qu'une des premisses n'est pas validée
            (setq conditions (cdr conditions))
            )))
      (if (and (eq test 0) (not (find conclusion *BDF* :test #'comparer-listes)))
            T
        nil)
      )))


; (verifPremisse '(R1 ((type0 eq ART)) (type1 eq ASSO_ART)))


; Fonction qui permet de lister l'ensemble des possibilités de listes de la base de règle
(defun possibilites ()
  (let ((liste nil) (cond nil))
    (dolist (ele *BDR*)
      (setq cond (premisseRegle ele))
      (dolist (ele2 cond)
        (if (not (find ele2 liste :test #'comparer-listes))
            (push ele2 liste))
        (pop ele2)
        )
      )
    liste))

; (possibilites)


; FOnction qui permet à partir de la valeur de retrouver la liste (type op valeur) dans la liste en paramètre
(defun find-by-value (val liste)
  (dolist (ele liste)
    (if (numberp val)
        (return (list 'nb 'eq val))
      (if (equal (car (last ele)) val) 
          (return ele)))))


; Fonction qui va appliquer toutes les règles possible avec la base de faits actuelle
(defun chainage_avant()
  (let ((cpt T) (reglesParcourues nil))
    (while cpt
      (dolist (regle *bdr*)
        (if (and (verifPremisse regle) (not (member (numRegle regle) reglesParcourues))) ; on vérifie que la règle peut être valider et qu'elle n'a pas déjà été validée
                 (progn 
                   (push (numRegle regle) reglesParcourues)
                   (if (not (cdr (assoc (car (cclregle regle)) *bdf*)))
                       (push (cclregle regle) *bdf*) ; ajout de la conclusion dans la BdF
                     ))
          (setq cpt nil) ; indique qu'il ne faut pas reboucler sur les règles
          ))
    ))
  )


; Algorithme principal du chaînage avant
(defun main_avant ()
  (setq *BDF* nil)
  (let ((choix nil) (liste nil) (tmp nil) (tmp_choix nil))
    (loop while (not (member choix (caddr (eval (car *BDQ*))))) do ; boucle pour poser la première question
          (print (cadr (eval (car *BDQ*))))
          (setq choix (read))
          (setq tmp_choix (find-by-value choix (possibilites)))) ; variable qui va contenir la liste à ajouter à la base de faits
    (setq liste tmp_choix) ; obligation de passer par une autre variable sinon tente d'évaluer lors du push
    (setq choix nil)
    (push liste *BDF*)
    (chainage_avant)
    (dolist (ele *BDQ*)
      (if (not (equal (caar *BDF*) 'asso))
          (progn
            (setq tmp (car (last (car *BDF*))))
            (if (equal (car (eval ele)) tmp) ; recherche de la question à poser
                (progn
                  (loop while (not (member choix (caddr (eval ele)))) do
                        (print (cadr (eval ele)))
                        (setq choix (read))
                        (setq tmp_choix (find-by-value choix (possibilites))))
                  (setq liste tmp_choix)
                  (setq choix nil)
                  (push liste *BDF*)
                  (chainage_avant)
                  (if (equal (caar *BDF*) 'asso) ; test de s'il faut s'arrêter
                      (progn
                        (format t "~%D'après les informations que vous nous avez fournit, l'association qui vous convient le mieux est :")
                        (return (car (last (assoc 'ASSO *BDF*)))))))
              )))
        ))
    
    )
        


;(main_avant)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




; Fonction qui va donner l'ensemble des règles permettant d'atteindre le but en paramètre
(defun regles_candidates (but)
  (let ((result nil))
    (dolist (regle *BDR*)
      (if (equal (cclRegle regle) but)
          (push regle result)))
    result))


;(regles_candidates '(asso eq BDBDEC))


; Fonction qui va permettre de retourner les prémisses du but entré en paramètre
(defun chainage_arriere (but)
  (let ((regles (regles_candidates but)))
    (if (not regles)
        nil
        (premisseRegle (car regles)))))


; (chainage_arriere '(asso eq BDBDEC))


(defvar *liste* nil)
(setq *liste* nil)


; Algorithme principal du chaînage arrière
(defun main_arriere (but)
  (let ((conditions (chainage_arriere but)) (test T) (val nil))
        (if conditions
            (dolist (condition conditions)
              (setq val (car (last (assoc (car condition) *bdf*))))
              (if (and val (funcall (cadr condition) val (car (last condition)))); si la condition ne peut pas être validée avec le contenu de la BdF on va chercher si elle peut être déduite d'une autre règle
                  nil
                (main_arriere condition))
              )
          (if (not (find but *BDF* :test #'comparer-listes)) ; si elle ne peut pas être déduite, on la met dans la variable *liste*
              (push but *liste*)))))
          

(main_arriere '(asso eq japonutc))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



; Fonction principale du TP
(defun main ()
  (let ((choix nil) (associations (liste_associations))) 
    (format t "Bienvenue sur Choisis ton Asso ! Veuillez sélectionner le mode que vous souhaitez utiliser : ~%chainage AVANT ou ARRIERE~%")
    (while (not (or (eq choix 'ARRIERE) (eq choix 'AVANT)))
      (setq choix (read))
      )
    (if (eq choix 'AVANT)
        (main_avant)
      (progn
        (setq *liste* nil)
        (setq choix nil)
        (format t "Voici la liste des associations disponibles :")
        (dolist (asso associations)
          (print asso))
        (format t "~%Veuillez entrer le nom de l'asso désirée : ")
        (while (not (member choix associations))
          (setq choix (read)))
        (setq choix (list 'asso 'eq choix))
        (main_arriere choix)
        (if *liste*
            "Malheureusement, cette association ne convient pas à vos critères."
          "Cette association convient bien à vos critères !")
          
        ))))


(main)