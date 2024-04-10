;; Exercice 1

;;(setq ll '( A 1 BB 2 CCC 3 DDD 4))

(defun f1 (L)
  (if L
      (progn (print (pop L))
        (f1 L))))

(defun f2 (L)
  (mapcar #'print L))

(defun f3 (L)
  (loop for x in L
        do (print x)))

(defun f4 (L)
  (dolist (x L)
    (print x)))

(defun f5 (L)
  (dotimes (i (length L))
    (print (pop L))))

(defun f6 (L)
  (loop
    (print (pop L))
    (if (not L) (return-from NIL))))


;; Exercice 2


;; (defvar *html* '(html (header (title "Ma page")) (body (h1 "Un titre") (p "Soror et aemula Romae"))))



(defun make-html (L i)
  (if (listp L)
      (progn
        (format t "~V@t<~s>~&" i (car L))
        (dolist (x (cdr L))
          (make-html x (+ i 3)))
        (format t "~V@t</~s>~&" i (car L))
        )
    (format t "~V@t~s ~&" i L)
    )        
  )


(defun fichier-html (liste nom)
  (with-open-file (file nom
                        :if-does-not-exist : create
                        :if-exists : overwrite
                        :direction : output)
    (make-html liste 0)
    )
  "fichier généré"
  )
  