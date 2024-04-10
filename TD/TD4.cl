; 1.
; Ensemble d'état (toutes les configurations que peut prendre le robot :
;     toutes les cases du labyrinthe) => 22 états

; Etats initiaux : positions du robot à l'entrée et à la sortie du labyrinthe 
;     (quand il bouge pas)

: Ensemble d'etats solution : le chemin (les cases) à parcourir pour sortir du
;     labyrinthe

; Ensemble d'action : ensemble des états dans lesquels on peut aller (H - B - G - D)


; 2.

; On va représenter le labyrinthe sous la forme d'une liste où chaque élément
; sera une liste contenant la case actuelle et les cases ateignables (rangées
; dans l'ordre croissant. (à noter que le premier élément de la première liste 
; sera un E pour "entrée" et le dernier sera un S pour "sortie".

; (setq lab '((E 1)(1 2)(2 7)(3 6)(4 5)(5 4 12)(6 3)(7 2 6 8)(8 7 9)(9 8 10)
;             (10 9 11 15)(11 10 12 14)(12 5 11)(13 20)(14 11)(15 10 16)
;             (16 17)(17 16 18)(18 17 19)(19 18 20)(20 13 19 S)(S 20))) -> va pas parce que dépend de la case d'avant



(setq lab '((E 1)(1 E 2)(2 1 7)(3 6)(4 5)(5 4 12)(6 3)(7 2 6 8)(8 7 9)(9 8 10)(10 9 11 15)(11 14)
            (12 5)(13 20)(14 11)(15 10 16)(16 15 17)(17 16 18)(18 17 19)(19 18 20)(20 13 S)(S 20)))



; 3.



; 4.



; 5.

(defun successeurs (etat lab)
  (cdr (assoc etat lab))
  )


(defun successeurs_valides (etat lab chem)
  (let ((succ (successeurs etat lab)))
    (dolist (x chem succ)
      (if (member x succ)
          (setq succ (remove x succ)))))
        )
    
   
(defun successeurs_valides (etat lab chem)
  (let ((succ (successeurs etat lab)) suivants)
    (dolist (x succ suivants)
      (if (not (member x chem))
          (push x suivants)
        ))))
    
    
    
(defun explore_prof (etat sortie lab chem)
  (
    
    
    
    
    
    
    
    
    
    
    
    