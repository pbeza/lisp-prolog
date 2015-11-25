; TODO ogarnąć czemu nie działa np. (d 'x '(* 1 (+ x x)))


(defun remove-brackets (lst)
  "Reduces lists with just one item to the item itself"
  (if (or (not (consp lst))
          (not (null (cdr lst))))
    lst
    (remove-brackets (car lst))))

(defun d (x E)
  "Różniczkuje zadane wyrażenie E składające się ze zmiennej x"
  (print E)
  (cond
    ((integerp E) 0)
    ((equalp E x) 1)
    ((equalp (length (write-to-string E)) 1) E)
    ((equalp '+ (car E)) (diff-sum x E))
    ((equalp '* (car E)) (diff-product x E))
    ((and
        (equalp (length E) 1)
        (equalp (car E) x)
    ) 1)
    ;((null E) 1)
    ;((listp E) (diff-variable x (remove-brackets E)))
    (t (print "Error: cannot parse expression.") (print E))
  )
)

; Pochodna sumy to suma pochodnych.
; d (E1 + E2) = d E1 + d E2
(defun diff-sum (x E)
  "Różniczkuje sumę wyrażeń"
  (print "diff-sum")
  (make-sum
    (map
      'list
      (lambda (expr) (d x expr))
      (cdr E))))

; d E1 E2 = E1 d E2 + E2 d E1
(defun diff-product (x E)
  "Różniczkuje iloczyn wyrażeń"
  (print "diff-product")
  (let(
    (E1 (car (cdr E)))              ; First factor
    (E2 (cons '* (cdr (cdr E)))))   ; (* Other factors)
    (if (equalp (length E2) 2)
      (setf E2 (cdr E2)))
  (setf dE1 (d x E1))               ; d E1
  (setf dE2 (d x E2))               ; d E2
  (
    cons '+
    (list
      (cons '* (list E1 dE2))
      (cons '* (list E2 dE1))))))

; Some basic simplification.  Not easy to do "completely," but this helps
; a lot.

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
  (simpl sum? make-sum 0 E))

(defun simplify-product (E)
  (simpl product? make-product 1 E))

; Here's the simplifier.
(defun simpl (isit? addop ident E)
  (let*
    (
      (parts (cdr E))			; Terms or factors.
      (sparts (map simplify parts))	; Terms or factors simplified.
      (fparts (flat isit? sparts))	; Simp (* x (* y z)) to (* x y z)
      (zout (replace-zero fparts))	; Reduce (* ... 0 ...) to 0.
      (unid (select (lambda (x) (not (equalp x ident))) zout))
      					; Remove identity (0 for + 1 for *)
    )
    (proper addop ident unid)))		; Cleanup; see below.

; The flat function looks for subexpressions of the same operator and merges
; them in.  For instance, change (+ x y (+ z w) (+ q 4) g) to
; (+ x y z w q 4 g).
(defun flat (isit args)
  (cond
    ((null? args) ())			; Empty is empty.
    ((not (pair? args)) (list args))	; I don't see how this happens, but ok.
    ((isit (car args))			; If first arg same op, combine.
      (append (flat isit (cdar args)) (flat isit (cdr args)))
    )
    (t					; Default: go on to the next.
      (cons (car args) (flat isit (cdr args))))))

; This simply adds the operator back to a list of terms or factors, but it
; avoids turning the empty list into (+) or the singleton list int (* 17).
(defun proper (addop ident args)
  (cond
    ((null? args) ident)		; () becomes 0 or 1.
    ((null? (cdr args)) (car args))	; (x) becomes x
    (t (addop args))))			; (x y z) to (+/* x y z)

; See if the expression is a multiplication containing zero (hence equal
; to zero).
(defun is-zero-mult? (E)
  (and (product? E)
    (not (equalp (select (lambda (x) (equalp x 0)) (cdr E)) ()))))

; Replace zero-valued multiply items with zero.  This works only on
; the top level.
(defun replace-zero (list)
  (if (null? list) ()
    (cons (if (is-zero-mult? (car list)) 0 (car list))
      (replace-zero (cdr list)))))
