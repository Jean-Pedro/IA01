; Solution 1 :
(setq BdR '((F (B D E) R1)(A (D G) R2)(A (C F) R3)(D (C) R4)(E (D) R5)(H (A) R6)(X (B) R7)(A (X C) R8)))


(defun CCL (regle)
  (car regle))

(defun premisse (regle)
  (cadr regle))

(defun numRegle (regle)
  (caddr regle))

(defun ReglesCandidates (but regles)
  (if regles
      (if (equal but (CCL(car regles)))
          (cons (car regles)(ReglesCandidates but(cdr regles)))
        (ReglesCandidates but (cdr regles))
        )))














(defun chainage-arriere-prof (but regles)
  (if (

 




; Solution 2 :
(setq 'BdR '((F ((B D E) R1))(A ((D G) R2)((C F) R3)((X C) R8))(D ((C) R4))(E ((D) R5))(H ((A) R6))(X ((B) R7))))

(defun ReglesCandidates (but regles)
  (cdr (assoc (but regles))))



