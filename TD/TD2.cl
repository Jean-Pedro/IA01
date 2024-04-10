(defun deriv-terme (exp variable)
  (if (eq exp variable)
      1
    0
    )
  )


(defun deriv-add-sous (exp variable)
  (if (listp exp)
      (list (car exp) (deriv (cadr exp) variable) (deriv (caddr exp) variable))
    (deriv-terme exp var)))



(defun deriv-multiplication (exp variable)
  (if (listp exp)
      (list '+ (list '* (deriv (cadr exp) variable) (caddr exp)) (list '* (cadr exp) (deriv (caddr exp) variable)))
    (deriv-terme exp variable)))


(defun deriv-division (exp variable)
  (if (listp exp)
      (list '/ (list '- (list (deriv (cadr exp) variable) (caddr exp)) (list (cadr exp) (deriv (caddr exp) variable))) (list '* (caddr exp) (caddr exp)))
    (deriv-terme exp variable)))


(defun deriv (exp variable)
  (if (listp exp)
      (cond
       ((eq (car exp) '+) (deriv-add-sous exp variable))
       ((eq (car exp) '-) (deriv-add-sous exp variable))
       ((eq (car exp) '*) (deriv-multiplication exp variable))
       ((eq (car exp) '/) (deriv-division exp variable)))
    (deriv-terme exp variable)))



(defun simpl (e)
  (if (listp e)
      (cond
       ((and (numberp (cadr e)) (numberp (caddr e))) (eval e))
       ((or (and (eq (car e) '+) (eq (caddr e) 0)) (and (eq (car e) '-) (eq (caddr e) 0))) (cadr e))
       ((and (eq (car e) '+) (eq (cadr e) 0)) (caddr e))
       ((and (eq (car e) '*) (or (eq (caddr e) 0) (eq (cadr e) 0))) 0)
       ((and (eq (car e) '*) (eq (cadr e) 1)) (caddr e))
       ((and (eq (car e) '*) (eq (caddr e) 1)) (cadr e))
       ((and (eq (car e) '/) (eq (cadr e) 0)) 0)
       (t e)
       ) 
    nil)
  )
