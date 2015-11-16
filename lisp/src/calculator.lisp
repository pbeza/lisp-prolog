; Symbolic calculator.

(defvar *separators* (list '+ '- '* '/) "Default operators for the math macro") 

(defun remove-brackets (lst)
    "Reduses lists with just one item to the item itself"
    (do ((result lst (car result)))
        ((or (not (consp result))
        (not (null (cdr result)))) result)))

(defun separate-list (lst separator test)
    "Returns list of sub-sequences defined by separator"
    (if (not (consp lst))
        lst
        (let ((result (cons separator nil)) (end 0) (sub)
        (lst (if (funcall test (car lst) separator)
            (cdr lst)
            lst)))
    (do () ((null lst) result)
        (setf end
        (position separator lst :test test))
	  (setf sub
		(cons (subseq lst 0 end) nil))
	  (setf result
		(append result sub))
	  (setf lst
		(if end
		    (nthcdr (+ 1 end) lst)
		    nil)))
	(setf (cdr result) (mapcar #'remove-brackets (cdr result)))
	result)))

(defun separate-tree (lst separator test)
    "Apply separate-list on all sublists"
    (if (or (not (consp lst)) (eql (first lst) 'quote))
        lst
        (progn
    (setf lst (mapcar #'(lambda (x)
			      (if (not (consp x))
				  x
				  (separate-tree x separator test)))
			  lst))
    (if (not (find separator (rest lst)))
        lst
        (separate-list lst separator test)))))

(defun infix->prefix (infix-expr separators &key (test #'eql))
    "Converts an infix expression to prefix"
    (let ((result infix-expr))
    (dolist (sep separators)
        (setf result (separate-tree result sep test)))
    (remove-brackets result)))

(defun insert-between (lst sep)
    (if (or (not (consp lst))
        (not (rest lst)))
        lst
    (cons (first lst) (mapcan #'(lambda (x) (list sep x)) (rest lst)))))

(defmacro !! (&body body)
    "Converts infix to prefix"
    (infix->prefix body *separators*))

(defun calc (infix-expr)
    "Calculate result of given expression"
    (eval (infix->prefix infix-expr '(+ - * /))))
