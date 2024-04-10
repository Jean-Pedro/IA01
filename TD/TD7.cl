(setq laby ((A1 B1 A2)(A2 A1 A3)(A3 A2 A4)(



(defun x (case)
  (- (char-int(char(symbol-name case) 0)) 64))

(defun y (case)
  (- (char-int(char(symbol-name case) 1)) 48))


(defun manathan (sP eP)
  (+ (abs (- (x sP)(x eP)))(abs (- (y sP)(y eP)))))


(defun successeurs-valides (case laby chemin)
  (let ((succ (cdr (assoc case carte))))
    (remove-if #'(lambda (x) (member x chemin)) succ)))


(defun glouton (sP eP laby chemin)
  (let ((successeurs (successeurs-valides (sP laby chemin))))
    (