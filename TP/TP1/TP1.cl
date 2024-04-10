;; Exercice 1

(defun reverseA (arg1 arg2 arg3)
  (list arg3 arg2 arg1))


(defun reverseB (L)
  (if (and (listp L)(<= (length L) 3)) ; v�rification des param�tres
      (cond 
       ((= (length L) 3) (list (caddr L) (cadr L) (car L))) ; inversion si taille = 3
       ((= (length L) 2) (list (cadr L) (car L))) ; inversion si taille = 2
       (t L)) ; renvoie juste la liste si taille = 1
    "Erreur dans la saisie")
  )


(defun reverseC (L)
  (if (listp L)
      (if (null L) '() ; condition d'arr�t
        (append (ReverseC (cdr L)) (list (car L))) ; met le premier �l�ment en dernier
        ) 
    "Erreur dans la saisie"))


(defun double (L)
  (if (listp L)
      (cond 
       ((null L) nil) ; condition d'arr�t
       ((atom (car L)) ; test de si le premier �l�ment de la liste est un atome
        (append (list (car L) (car L)) (double (cdr L))) ; ajoute 2 fois l'�l�ment � la liste et appel r�cursif sur le reste
        )
       (T (cons (car L) (double (cdr L)))) ; si pas un atome ajoute seulement une fois
       ) 
    "Erreur dans la saisie")
  )


(defun nombres3 (L)
  (if (listp L)
      (cond 
       ((and (numberp (car L))(numberp (cadr L))(numberp (caddr L))) 'BRAVO) ; test des 3 premiers �l�ments de la liste
       (T 'PERDU)) ; affiche perdu si pas des nombres
    "Erreur dans la saisie")
  )


(defun grouper (L1 L2)
  (if (and (listp L1)(listp L2))
      (if (and L1 L2) ; test que les deux listes sont non vides
          (cons (list (car L1) (car L2)) (grouper (cdr L1)(cdr L2)))) ; cr�ation de la liste
    "Erreur dans la saisie")
  )
        


(defun monReverse (L)
  (if (listp L)
      (if (null L) '() ; condition d'arr�t
        (append (monReverse (cdr L)) (if (listp (car L)) (list (reverseC (car L))) (list (car L)))) ; cr�ation de la liste invers�e par r�curence
        )
    "Erreur dans la saisie")
  )


(defun palindrome (L)
  (if (listp L)
      (if (equal (monReverse L) L) T 'F) 
    ; utilisation de la fonction monReverse pour obtenir l'inverse de la liste en param�tre, puis test d'�galit� avec la liste L
    "Erreur dans la saisie")
  )


; Jeux de test

(reverseA 'a 'b 'c)
(reverseB '(5 6))
(reverseC '(1 2 3 (4 5) 6))
(double '(1 2 3 (4 5) 6 (7 8)))
(nombres3 '(1 2 3 g e r))
(nombres3 '(g 1 2 3 e r))
(grouper '(1 2 3) '(4 5 6))
(monReverse '(1 2 (3 4) 5 (6 7)))
(palindrome '(x a m a x))
(palindrome '(b o n j o u r))


;; Exercice 2

(defun list-triple-couple (L)
  (if (listp L) ; test de la conformit� du param�tre
      (mapcar #'(lambda (L) (list L (* 3  L))) L) ; utilisation d'un mapcar pour it�rer sur la liste et d'un lambda pour cr�er les listes demand�es
    "Erreur dans la saisie")
  )

; Jeu de test

(list-triple-couple '(1 2 3))


;; Exercice 3

(defun my-assoc (cle a-list)
  (if (listp a-list) ; test de la conformit� des param�tres
      (if (not (equal (car a-list) nil)) ; v�rification que la liste n'est pas vide (= condition d'arr�t de la r�cursivit�)
          (if (equal (car (car a-list)) cle) (car a-list) (my-assoc cle (cdr a-list)))) ; v�rification de la correspondance avec la cl� en param�tre et appel r�cursif
    "Erreur dans la saisie")
  )
  
  
(defun cles (a-list)
  (if (listp a-list) ; test de la conformit� du param�tre
      (mapcar #'car a-list) "Erreur dans la saisie")) ; it�re sur chaque �l�ment de la liste et retourne son car (c'est-�-dire la cl�)
  

(defun creation (listeCles listeValeurs)
  (if (and (listp listeCles)(listp listeValeurs)) ; test de la conformit� des param�tres
      (mapcar #'(lambda (cle valeur) (list cle valeur)) listeCles listeValeurs) "Erreur dans la saisie"))
; utilisation d'un mapcar pour it�rer sur chaque �l�ments des deux listes et cr�ation de la liste avec une fonction lambda


; Jeu de test
(my-assoc 'Pierre '((Yolande 25) (Pierre 22) (Julie 45)))
(my-assoc 'Yves '((Yolande 25) (Pierre 22) (Julie 45)))
(cles '((Yolande 25) (Pierre 22) (Julie 45)))
(creation '(Yolande Pierre Julie) '(25 22 45))



;; Exercice 4

; donn�es que l'on va utiliser pendant l'exercice :

(setq tombeBecaud '("B�caud" 2001 (45 17) 2000 30))
(setq tombeZavatta '("Zavatta" 1993 (11 16) 1993 15))
(setq cimetieres '((marin (("B�caud" 2001 (45 17) 2000 30)))
                   (pere-lachaise (
 ("B�caud" 2001 (45 17) 2000 30) ("Desproges" 1989 (11 6) 1988 30)
 ("Grappelli" 1997 (85 23) 1997 5) ("Jacob" 1968 (107 8) 1968 5) 
 ("Morrison" 1971 (6 12) 1971 30)
 ("Mouloudji" 1994 (42 9) 1990 15) ("Nohain" 1981 (89 14) 1979 15)
 ("Oussekine" 1986 (85 37) 1986 5) ("Petrucciani" 1999 (11 26) 1999 15)
 ("Popesco" 1993 (85 16) 1985 30) ("Signoret" 1985 (44 7) 1980 30)
 ("Zavatta" 1993 (11 16) 1993 15)))
                   ))
; la variable cimetieres contient 2 cimeti�res diff�rents qui nous on permit de faire diff�rents tests



;; Question 1

(defun nom (tombe)
  (if (listp tombe) (car tombe) "Erreur dans les param�tres"))

(defun an-inhum (tombe)
  (if (listp tombe) (cadr tombe) "Erreur dans les param�tres"))

(defun num (tombe)
  (if (listp tombe) (car (cdaddr tombe)) "Erreur dans les param�tres"))

(defun rangee (tombe)
  (if (listp tombe) (caaddr tombe) "Erreur dans les param�tres"))

(defun debut-loc (tombe)
  (if (listp tombe) (cadddr tombe) "Erreur dans les param�tres"))

(defun duree-loc (tombe)
  (if (listp tombe) (car (last tombe)) "Erreur dans les param�tres"))


; Jeu de tests

(nom tombeZavatta)
(an-inhum tombeZavatta)
(num tombeZavatta)
(rangee tombeZavatta)
(debut-loc tombeZavatta)
(duree-loc tombeZavatta)


;; Question 2

(defun qui-est-la (emplacement cimetiere listeC)
  (if (and (and (listp emplacement)(and (numberp (car emplacement))(numberp (cadr emplacement))))(listp listeC)) ; tests des param�tres
      (let ((v nil)) ; initialisation de la variable qui va contenir le nom de la personne
        (dolist (x (cadr (assoc cimetiere listeC)))
          (if (equal emplacement (caddr x)) (setf v (nom x)))) ; pour chaque tombe, test de si l'emplacement et le m�me et r�cup�ration du nom si c'est le cas       
        (if v v "Emplacement non attribu�")) ; affichage
  "Erreur dans la saisie"))

; Jeu de test

(qui-est-la '(11 16) 'pere-lachaise cimetieres)


;; Question 3

(defun prevoyant? (tombe) 
    (< (debut-loc tombe) (an-inhum tombe))
  )

; Jeu de test

(prevoyant? tombeBecaud)
(prevoyant? tombeZavatta)


;; Question 4


(defun nb-prevoyants (nom-cimetiere cimetiere)
  (if (listp cimetiere) ; test de la conformit� des param�tres
      (let ((a 0)) ; variable qui va servir de compteur
        (loop for i in (cadr (assoc nom-cimetiere cimetiere))
            do (if (prevoyant? i) (setf a (+ a 1)))) ; on teste pour chaque personne si elle a �t� prevoyante avec la fonction d�finie pr�cedemment
        a ; affichage
        ) "Erreur dans la saisie")
  )

; Jeu de test

(nb-prevoyants 'pere-lachaise cimetieres)


;; Question 5

(defun annuaire (nom-cimetiere rang cimetiere)
  (if (and (numberp rang)(listp cimetiere)) ; test de la conformit� des param�tres
      (let ((l '())) ; d�finition de la liste qui va servir d'annuaire
        (loop for i in (cadr (assoc nom-cimetiere cimetiere))
            do (if (equal (rangee i) rang) (setf l (append l (list (nom i)))))) ; on teste si la rang�e correspond et si c'est le cas on ajoute la personne � la liste
        l ; affichage
        ) "Erreur dans la saisie")
  )

; Jeu de test

(annuaire 'pere-lachaise 85 cimetieres)


;; Question 6

(defun doyen-benjamin (nom-cimetiere cimetiere)
  (if (listp cimetiere) ; test de la conformit� des param�tres
      (let ((d (caadr (assoc nom-cimetiere cimetiere))) (b (caadr (assoc nom-cimetiere cimetiere))) (db '())) ; d�finition des 3 variables
        (loop for i in (cadr (assoc nom-cimetiere cimetiere))
            do (if (< (an-inhum i) (an-inhum d)) (setq d i)) ; recherche du doyen
              (if (> (an-inhum i) (an-inhum b)) (setq b i))) ; recherche du benjamin
        (setq db (list d b)) ; association des deux pour l'affichage
        ) "Erreur dans la saisie")
  )

; Jeu de test

(doyen-benjamin 'pere-lachaise cimetieres)
          