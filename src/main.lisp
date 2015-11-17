;;;; Common Lisp infix/prefix conversion utility
;;;
;;;  $Id: infpre.lisp,v 1.2 2006/11/19 16:01:52 jornv Exp $
;;;
;;;; Licence LGPL
;;;
;;;; Copyright: Joern Inge Vestgaarden (jivestgarden at gmail com)
;;; 
;;;; Syntax: 
;;;   Works directly on lisp lists, not on strings.
;;;   The cost is that all operators must be separated by spaces, 
;;;   i.e. 1 + 2, not 1+2. 
;;;   
;;;   Unlike most infix utilities, the infix conversion
;;;   does not interpret +,*, etc. as binary operators,
;;;   but as list separted by the operator
;;;   i.e. (1 + 2 + 3) -> (+ 1 2 3) not (+ (+ 1 2) 3). 
;;;
;;;   The order of the operators determine precedence. 
;;;
;;;; Examples:
;;;   (1 + 2 * exp (-1 * x) * 3) -> (+ 1 (* 2 (exp (* -1 x)) 3))
;;; 
;;;; Bugs: 
;;;   Works directly on CL symbols which cause problems with packages.
;;;   The math macro only works because +-*/ are speical variables
;;;   in the common-lisp package. In general a new test-function
;;;   working on names must be made and supplied.
;;;

;(defpackage "INFPRE"
;  (:use "COMMON-LISP")
;  (:export "INFIX->PREFIX"
;       "PREFIX->INFIX"
;       "MATH"
;       "!!")
;  (:documentation ""))

;(in-package :infpre)

(defvar *separators* (list '+ '- '* '/) "Default operators for the math macro") 

(defun remove-brackets (lst)
  "Reduces lists with just one item to the item itself"
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

;;;; End of infix prefix conversion


;;;; Additional useful macros as interfaces to infix->prefix

(defun !! (body)
    "Converts infix to prefix"
    (infix->prefix body *separators*))


(defmacro macro-factorial (n)
    "Macro factorial"
    (if (= 0 n)
        '1
        (let ((m (1- n)))
            `(* ,n (macro-factorial ,m)))))

(defun factorial (n)
    "Function factorial"
    (case n
        (0 1)
        (10 (macro-factorial 10))
        (otherwise (* n (factorial (1- n))))))

(defun plus (&rest args)
    (apply '+ args))

(defun minus (&rest args)
    (apply '- args))

(defun multiply (&rest args)
    (apply '* args))

(defun divide (&rest args)
    (apply '/ args))

(defun rename-one (item)
    (case item
        ('+ #'plus)
        ('- #'minus)
        ('* #'multiply)
        ('/ #'divide)
        ('factorial #'factorial)
        (t item)))

(defun rename-functions (expr)
    "Changes symbols +, -, * and / to respective functions"
    (if (null expr)
        nil
        (let ((item (first expr)))
            (if (consp item)
                (cons (rename-functions item) (rename-functions (rest expr)))
                (cons (rename-one item) (rename-functions (rest expr)))))))

(defun precalc (expr)
    "Calculates nested expressions inside this one."
    (if (subtypep (type-of (first expr)) 'null)
        nil
        (if (consp (first expr))
            (cons (calc (first expr)) (precalc (rest expr)))
            (cons (first expr) (precalc (rest expr))))))

(defun postcalc (expr)
    "Calculates this expression, assuming it has no nested expressions."
    ;(write expr)
    (write-line "")
    (write (first expr))
    (write-line "")
    (write (rest expr))
    (write-line "")
    (let ((frst (rename-one (first expr))))
        (if (subtypep (type-of frst) 'function)
            (apply frst (rest expr))
            expr)))

(defun calc (expr)
    (postcalc (precalc expr)))

;(trace !!)
;(trace infix->prefix)
(trace rename-functions)
;(trace calc)
(trace precalc)
(trace postcalc)
(compile 'factorial)
(compile 'plus)
(compile 'multiply)
(compile 'precalc)
(compile 'postcalc)

(makunbound '*example_list*)
;(defvar *example_list* '(3 + (2 + 5) * 2 * 1 * 'factorial 5 * 2))
(defvar *example_list* '(3 + 'factorial 5 * 2))
(write *example_list*)
(write-line "")
(write (infix->prefix *example_list* *separators*))
(write-line "")
;(let ((lll '(3 + (2 + 5) * 2)))
;    (write (!! lll)))
;(write-line "")
;(write (macroexpand '(!! *example_list* + *example_list* * 2)))
;(write-line "")
;(write (macroexpand '(!! (let ((a 42)) a - 2 * 2 * 10))))
;(write-line "")
;(write (macroexpand '(macro-factorial 7)))
;(write-line "")
;(write (macro-factorial 7))
;(write-line "")
;(let ((lll (read)))
(let ((lll *example_list*))
    (write (!! lll))
    (write-line "")
    (write (!! lll))
    (write (calc (!! lll)))
    ;(write (rename-functions (!! lll)))
    ;(write (calc (rename-functions (!! lll))))
    ;(write (calc (!! lll)))
    ;(write (calc (rename-simple (!! lll))))
)
(write-line "")
;(format t "~a~%" *example_list*)
;(format t "~a~%" (!! *example_list*))
