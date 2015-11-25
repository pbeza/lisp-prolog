;------------------------------------------------------------------------------
; Konwersja postaci infixowej wyrażenia na prefixową.
;------------------------------------------------------------------------------


(defvar *separators* (list '+ '- '* '/) "Domyślnie rozpoznawane operatory")

(defun remove-brackets (lst)
    "Redukuje listę z jednym elementem do tego elementu."
    (if (or (not (consp lst))
            (not (null (cdr lst))))
      lst
      (remove-brackets (car lst))))

(defun separate-list (lst separator test)
    "Zwraca listę podlist zdefiniowanych przez separator."
    (if (not (consp lst))
        lst
        (let (
                (result (cons separator nil))
                (end 0)
                (sub)
                (lst (if (funcall test (car lst) separator)
                        (cdr lst)
                        lst)
                )
            )
            (do () ((null lst) result) ; warunek końcowy i zwracana wartość
                (setf end (position separator lst :test test))
                (setf sub (cons (subseq lst 0 end) nil))
                (setf result (append result sub))
                (setf lst (if end (nthcdr (+ 1 end) lst) nil))
            )
            (setf (cdr result) (mapcar #'remove-brackets (cdr result)))
            result
        )
    )
)

(defun separate-tree (lst separator test)
    "Wywołaj separate-list na wszystkich podlistach"
    (if (or (not (consp lst)) (eql (first lst) 'quote))
        lst
        (progn
            (setf lst (mapcar #'(lambda (x)
                (if (not (consp x))
                    x
                    (separate-tree x separator test) ; sublist found
                )) lst)
            )
            (if (not (find separator (rest lst)))
                lst
                (separate-list lst separator test)
            )
        )
    )
)

(defun infix->prefix (infix-expr separators &key (test #'eql))
    "Zamienia postać infixową wyrażenia na prefixową"
    (let ((result infix-expr))
    (dolist (sep separators)
        (setf result (separate-tree result sep test)))
    (remove-brackets result)))

(defmacro !! (body)
    "Konwersja postaci infix na prefix z domyślnymi separatorami"
    `(infix->prefix ,body *separators*))


;------------------------------------------------------------------------------
; Operacje matematyczne.
;------------------------------------------------------------------------------


(defmacro macro-factorial (n)
    "Makro wykładnicze"
    (if (= 0 n)
        '1
        (let ((m (1- n)))
            `(* ,n (macro-factorial ,m)))))

(defun factorial (n)
    "Funkcja wykładnicza"
    (case n
        (0 1)
        (10 (macro-factorial 10))
        (otherwise (* n (factorial (1- n))))))

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


;------------------------------------------------------------------------------
; RÓŻNICZKOWANIE SYMBOLICZNE.
;------------------------------------------------------------------------------


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

(defun simpl (isit? addop ident E)
    (let*
        (
          (parts (cdr E))                    ; Terms or factors.
          (sparts (mapcar #'simplify parts)) ; Terms or factors simplified.
          (fparts (flat isit? sparts))       ; Simp (* x (* y z)) to (* x y z)
          (zout (replace-zero fparts))       ; Reduce (* ... 0 ...) to 0.
          (unid (remove-identity zout ident))
                                             ; Remove identity (0 for + 1 for *)
        )
        (proper addop ident unid)))          ; Cleanup; see below.

; The flat function looks for subexpressions of the same operator and merges
; them in.  For instance, change (+ x y (+ z w) (+ q 4) g) to
; (+ x y z w q 4 g).
(defun flat (isit args)
  (cond
    ((null args) ())                         ; Empty is empty.
    ((not (pair? args)) (list args))         ; I don't see how this happens, but ok.
    ((isit (car args))                       ; If first arg same op, combine.
      (append (flat isit (cdar args)) (flat isit (cdr args)))
    )
    (t                                       ; Default: go on to the next.
        (cons (car args) (flat isit (cdr args))))))

; This simply adds the operator back to a list of terms or factors, but it
; avoids turning the empty list into (+) or the singleton list int (* 17).
(defun proper (addop ident args)
  (cond
    ((null args) ident)                      ; () becomes 0 or 1.
    ((null (cdr args)) (car args))           ; (x) becomes x
    (t (addop args))))                       ; (x y z) to (+/* x y z)

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


;------------------------------------------------------------------------------
; Podstawowa funkcjonalność kalkulatora.
;------------------------------------------------------------------------------

(defun rename-one (item)
    "Mapowanie operacja -> nazwa funkcji."
    (case item
        ('quote nil)
        ('+ #'plus)
        ('- #'minus)
        ('* #'multiply)
        ('/ #'divide)
        ('factorial #'factorial)
        (t item)))

(defun rename-functions (expr)
    "Zamienia +, -, * i / na równoważne funkcje."
    (cond
        ((null expr) nil)
        ;((equal (first expr) 'quote) (first (rename-functions (rest expr))))
        ((consp (first expr)) (cons (rename-functions (first expr)) (rename-functions (rest expr))))
        (t (cons (rename-one (first expr)) (rename-functions (rest expr))))
    ))

(defun precalc (expr)
    "Oblicza zagnieżdżone wyrażenia wewnątrz expr."
    ;(write-line "Precalc")
    ;(write expr)
    ;(write-line "")
    (cond
        ((null (first expr)) nil)
        ((consp (first expr)) (cons (calc (first expr)) (precalc (rest expr))))
        (t (cons (first expr) (precalc (rest expr))))
    ))

(defun postcalc (expr)
    "Oblicza wyrażenie expr zakładając, że nie ma ono zagnieżdżonych wyrażeń."
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
(defvar *ex* '(3 * 2 * (factorial 5 + (0 * 'x * (0 * 'x))) + 2))

;(trace postcalc)
;(trace precalc)
(trace multiply)
(trace symbolic-reduce)

(calc-print *ex*)
;(main-loop)
