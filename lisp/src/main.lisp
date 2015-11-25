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

(defmacro !! (body)
    "Converts infix to prefix"
    `(infix->prefix ,body *separators*))


(defmacro macro-factorial (n)
    "Macro factorial"
    (if (= 0 n)
        '1
        (let ((m (1- n)))
            `(* ,n (macro-factorial ,m)))))

(defmacro macro-factorial (n)
    "Macro factorial"
    (cond
        ((not (numberp n)) `(factorial ,n))
        ((= 0 n) '1)
        (t (let ((m (1- n)))
            `(* ,n (macro-factorial ,m))))))

(defun factorial (n)
    "Function factorial"
    (cond
        ((not (numberp n)) `(factorial ,n))
        ((>= 1 n) 1)
        (t (let ((m (1- n)))
            (* n (factorial m))))))

(defun calculable (expr)
    (cond
        ((numberp expr) t)
        ((consp expr) (and (calculable (first expr)) (calculable (rest expr))))
        (nil t)
        (t nil)))

(defun symbolic-reduce (rsym rfun expr)
    (let* (
        (partitioned (partition #'numberp expr))
        (numbers (first partitioned))
        (symbols (second partitioned))
        (reduced (reduce rfun numbers))
    )
    ;(format t "~&~%~a~&~%~a" expr partitioned)
    (if (null symbols)
        reduced
        (append `(,rsym ,reduced) symbols))))

(defun partitionn (predicate list)
    (loop for x in list
        if (funcall predicate x) collect x into yes
        else collect x into no
        finally (return (values yes no))))

(defun partition (predicate expr)
    (reduce (lambda (a b)
                (if (funcall predicate a)
                        (push a (first b))
                    (push a (second b)))
                b)
            expr
            :initial-value (list nil nil)
            :from-end t))

(defun plus (&rest expr)
    (symbolic-reduce '+ #'+ expr))

(defun invert-sign(&rest args)
    ;TODO
    args)

(defun minus (&rest args)
    (plus (cons (first args) (invert-sign (rest args)))))

(defun multiply (&rest expr)
    (setf mult (symbolic-reduce '* #'* expr))
    (cond
        ((not (consp mult)) mult)
        ((>= 1 (length mult)) mult)
        ((equal (nth 1 mult) 0) 0)
        (t mult)))

(defun divide (&rest args)
    (apply '/ args))

; Tworzy sume.
(defun make-sum (x) (cons '+ x))

; Tworzy iloczyn.
(defun make-product (x) (cons '* x))

; Sprawdzenie czy suma
(defun sum? (E)
  (and (pair? E) (equalp '+ (car E))))

; Sprawdzenie czy iloczyn.
(defun product? (E)
  (and (pair? E) (equalp '* (car E))))

(defun pair? (E)
    (and (consp E) (equalp (length (cdr E)) 2)))

; Top-level simplify.
(defun simplify (E)
  (cond
    ((sum? E) (simplify-sum E))
    ((product? E) (simplify-product E))
    (t E)))

; The sum and product simplifiers are mainly calls to simpl, with some
; appropriate control parameters.  The parameters are the corresponding
; identifier and make- function, and the identity for that operation.
(defun simplify-sum (E)
  (simpl #'sum? #'make-sum 0 E))

(defun simplify-product (E)
  (simpl #'product? #'make-product 1 E))

(defun remove-identity (E ident)
    (maplist
        (lambda (x)
            (if (not (equalp (first x) ident))
                x
                nil))
        E))

; Here's the simplifier.
(defun simpl (isit? addop ident E)
    ;(write-line "Simpl")
    ;(write E)
    ;(write-line "")
    (let*
        (
          (parts (cdr E))               ; Terms or factors.
          (sparts (mapcar #'simplify parts)) ; Terms or factors simplified.
          (fparts (flat isit? sparts))  ; Simp (* x (* y z)) to (* x y z)
          (zout (replace-zero fparts))  ; Reduce (* ... 0 ...) to 0.
          (unid (remove-identity zout ident))
                                        ; Remove identity (0 for + 1 for *)
        )
        (proper addop ident unid))) ; Cleanup; see below.

; The flat function looks for subexpressions of the same operator and merges
; them in.  For instance, change (+ x y (+ z w) (+ q 4) g) to
; (+ x y z w q 4 g).
(defun flat (isit args)
  (cond
    ((null args) ())                   ; Empty is empty.
    ((not (pair? args)) (list args))    ; I don't see how this happens, but ok.
    ((isit (car args))                  ; If first arg same op, combine.
      (append (flat isit (cdar args)) (flat isit (cdr args)))
    )
    (t  ; Default: go on to the next.
        (cons (car args) (flat isit (cdr args))))))

; This simply adds the operator back to a list of terms or factors, but it
; avoids turning the empty list into (+) or the singleton list int (* 17).
(defun proper (addop ident args)
  (cond
    ((null args) ident)            ; () becomes 0 or 1.
    ((null (cdr args)) (car args)) ; (x) becomes x
    (t (addop args))))              ; (x y z) to (+/* x y z)

; See if the expression is a multiplication containing zero (hence equal
; to zero).
(defun is-zero-mult? (E)
    (and
        (product? E)
        (some (lambda (item) (equal 0 item)) E)
    ))
    

; Replace zero-valued multiply items with zero.  This works only on
; the top level.
(defun replace-zero (expr)
  (if (null expr) ()
    (cons (if (is-zero-mult? (car expr)) 0 (car expr))
      (replace-zero (cdr expr)))))


(defun rename-one (item)
    (case item
        ('quote nil)
        ('+ #'plus)
        ('- #'minus)
        ('* #'multiply)
        ('/ #'divide)
        ('factorial #'factorial)
        (t item)))

(defun rename-functions (expr)
    "Changes symbols +, -, * and / to respective functions"
    (cond
        ((null expr) nil)
        ;((equal (first expr) 'quote) (first (rename-functions (rest expr))))
        ((consp (first expr)) (cons (rename-functions (first expr)) (rename-functions (rest expr))))
        (t (cons (rename-one (first expr)) (rename-functions (rest expr))))
    ))

(defun precalc (expr)
    "Calculates nested expressions inside this one."
    ;(write-line "Precalc")
    ;(write expr)
    ;(write-line "")
    (cond
        ((null (first expr)) nil)
        ((consp (first expr)) (cons (calc (first expr)) (precalc (rest expr))))
        (t (cons (first expr) (precalc (rest expr))))
    ))

(defun postcalc (expr)
    "Calculates this expression, assuming it has no nested expressions."
    ;(write-line "Postcalc")
    ;(write expr)
    ;(write-line "")
    ;(write (first expr))
    ;(write-line "")
    ;(write (rest expr))
    ;(write-line "")
    (setf frst (first expr))
    (cond
        ((null (rest expr)) expr)
        ;((eq frst 'quote) (postcalc (rest expr)))
        ((subtypep (type-of frst) 'function) (apply frst (rest expr)))
        ((subtypep (type-of (rename-one frst)) 'function) (apply (rename-one frst) (rest expr)))
        ((and (consp frst) (eq (length frst) 1)) (car frst))
        (t expr)
    ))

(defun calc (expr)
    ;(postcalc (simplify (precalc (simplify expr)))))
    (postcalc (precalc expr)))

(defun calc-print (expr)
    (setf inlined (!! expr))
    ;(setf renamed (rename-functions inlined))
    (setf renamed inlined)
    (setf calced (calc renamed))
    (format t "~&~%~a~&~%~a~&~%~a~&~%~a~%" expr inlined renamed calced))

(defun main-loop ()
    (write-line "Input formula (or q to exit)")
    (loop while (setq inp (get-input))
        do (calc-print inp)))

(defun get-input ()
    (let ((inp (read)))
        (if (eq 'q inp)
            nil
            inp)))

(makunbound '*ex*)
(defvar *ex* '(3 * 2 * (factorial 5 + (0 * x * (0 * x))) + 2 + x + (factorial x)))

;(trace postcalc)
;(trace precalc)
(trace multiply)
(trace symbolic-reduce)

(calc-print *ex*)
;(main-loop)