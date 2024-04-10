(defvar *nodes* '())
(defvar *arcs* '())

(setq *nodes* nil)
(setq *arcs* nil)


; Nodes

(defun defnode (node type nom)
  (push
   (list node
         (list 'type type)
         (list 'nom nom)
         (list 'arcs_in)
         (list 'arcs_out)
         (list 'marks))
   *nodes*))


(defnode 'N0 'concept 'noble)
(defnode 'N1 'concept 'cadet)
(defnode 'N2 'individu 'christian)
(defnode 'N3 'concept 'mondaine)
(defnode 'N4 'individu 'roxane)

; Arcs


(defun defarc (arc type from to)
  (push
   (list arc
         (list 'type type)
         (list 'from_node from)
         (list 'to_node to))
   *arcs*)
  (push arc (cdr(assoc 'arcs_in (cdr (assoc to *nodes*)))))
  (push arc (cdr(assoc 'arcs_out (cdr (assoc from *nodes*))))))

(defarc 'A0 'aime 'N4 'N2)
(defarc 'A1 'aime 'N2 'N4)


(defun mark_node (node mark)
  (pushnew mark (cdr (assoc 'marks
                            (cdr (assoc node *nodes*))))))

(mark_node 'N2 'M1)
  

(defun is_marked (node mark)
  (if (member mark (cdr (assoc 'marks (cdr (assoc node *nodes*))))) 'T))

(is_marked 'N2 'M1)


(defun is_typed (arc type)
  (equal (list type) (cdr (assoc 'type (cdr (assoc arc *arcs*))))))


(is_typed 'A0 'aime)



(defun predecesseurs (node type)
  (let ((pred nil) (arcs_in (cdr (assoc 'arcs_in (cdr (assoc node *nodes*))))))
    (dolist (a arcs_in)
      (if (equal (list type) (cdr (assoc 'type (cdr (assoc a *arcs*)))))
          (push (cadr (assoc 'from_node (cdr (assoc a *arcs*)))) pred))) pred))
                 


(predecesseurs 'N4 'aime)




(defun successeurs (node type)
  (let ((succ nil) (arcs_out (cdr (assoc 'arcs_out (cdr (assoc node *nodes*))))))
    (dolist (a arcs_out)
      (if (equal (list type) (cdr (assoc 'type (cdr (assoc a *arcs*)))))
          (push (cadr (assoc 'to_node (cdr (assoc a *arcs*)))) succ))) succ))


(successeurs 'N4 'aime)



(defun wave (node mark type sens)
  (let ((suivant nil))
    (if (not (is_marked node mark))
      (progn (mark_node node mark) 
        (if (equal sens 'direct) (setq suivant (successeurs node type)) (setq suivant (predecesseurs node type)))
        (dolist (x suivant)
          (wave x mark type sens)))))*nodes*)


(wave 'N2 'M1 'aime 'direct)



(defun get_results (mark1 type mark2)
  (let ((results nil))
    (dolist (a *arcs*)
      (if (and (and (is_typed a type) (is_marked (cadr (assoc 'from_node a)) mark1)) (is_marked (cadr (assoc 'to_node a)) mark2))
          (push a results))))
        )





