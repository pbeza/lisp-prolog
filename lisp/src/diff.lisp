(defun remove-brackets (lst)
  "Reduces lists with just one item to the item itself"
  (if (or (not (consp lst))
          (not (null (cdr lst))))
    lst
    (remove-brackets (car lst))))

(defun d (x E)
  (cond
    ;((constant? E) (diff-constant x E))
    ((integerp E) (diff-constant x E))
    ;((variable? E) (diff-variable x E))
    ((equalp E x) (diff-variable x E))
    ((sum? E) (diff-sum x E))
    ((product? E) (diff-product x E))
    ((listp E) (diff-variable x (remove-brackets E)))
    (t (print "Error: cannot parse expression."))
  )
)

; Różniczkowanie stałej.
(defun diff-constant (x E) (print "diff-constant") 0)

; Różniczkowanie zmiennej (dx x = 1, dx y = 0).
(defun diff-variable (x E)
  (print "diff-variable")
  (if (equalp x E) 1 0))

(defun pair? (E)
  (print "pair?")
  (equalp (length (cdr E)) 2))

; Sprawdzenie czy suma
(defun sum? (E)
  (print "sum?")
  (and (pair? E) (equalp '+ (car E))))

; Sprawdzenie czy iloczyn.
(defun product? (E)
  (print "product?")
  (and (pair? E) (equalp '* (car E))))

; Tworzy sumę.
(defun make-sum (x) (cons '+ x))

; Tworzy iloczyn.
(defun make-product (x) (cons '* x))

; Pochodna sumy to suma pochodnych.
; d (E1 + E2) = d E1 + d E2
(defun diff-sum (x E)
  (print "diff-sum")
  (make-sum
    (map
      'list
      (lambda (expr) (d x expr))
      (cdr E))))

; Pochodna iloczynu to:
;   d E1 E2 = E1 d E2 + E2 d E1
; This is actually implemented by chain-rule; the diff-product function
; filters out the cases where the product has fewer than two factors.
(defun diff-product (x E)
  (let*
    ; See how many things we're got.
    ((nfact (length (cdr E))))
    (cond
      ; No factors.  Just a long-winded way to write the constant 1,
      ; whose differential is zero.
      ((equalp nfact 0) 0)
      ; One factor.  Not really much of a product.  Loose the multiply
      ; and take the differential of the single factor.
      ((equalp nfact 1) (d x (car (cdr E))))
      ; Real case.
      (t (chain-rule x E)))))

; Here's the actual chain rule function.
;   d E1 E2 = E1 d E2 + E2 d E1
(defun chain-rule (x E)
  (print "chain-rule")
  (print E)
  (let*
    (
      (E1 (car (cdr E)))		; First factor.
      (E2 (make-product (cdr (cdr E)))) ; (* Other factors)
      (dE1 (d x E1))			; d E1
      (dE2 (d x E2))			; d E2
    )
    (print "E1")
    (print E1)
    (print "dE2")
    (print dE2)
    (print "E2")
    (print E2)
    (print "dE1")
    (print dE1)
    (
     make-sum
      (list
        (make-product (list E1 dE2))
        (make-product (list E2 dE1)))
    )))

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
