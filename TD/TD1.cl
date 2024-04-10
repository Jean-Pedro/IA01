(defun infix_to_prefix (expression)
  (if (listp expression)
      (list (infix_to_prefix (cadr expression)) (infix_to_prefix (car expression)) (infix_to_prefix (caddr expression))) expression))