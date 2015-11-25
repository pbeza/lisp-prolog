;------------------------------------------------------------------------------
; Różniczkowanie symboliczne.
;------------------------------------------------------------------------------

; Funkcja rekurencyjna (P.B.)

(defun remove-brackets (lst)
    "Redukuje listę z jednym elementem do tego elementu."
    (if (or (not (consp lst))
            (not (null (cdr lst))))
        lst
        (remove-brackets (car lst))))

(defun d (x E)
    "Różniczkuje zadane wyrażenie E składające się ze zmiennej x."
    (setf E (remove-brackets E))
    (cond
        ((integerp E) 0)				; stała liczbowa
        ((equalp E x) 1)				; zmienna
        ((equalp (length (write-to-string E)) 1) E)	; stała "literowa"
        ((equalp '+ (car E)) (diff-sum x E))		; suma
        ((equalp '* (car E)) (diff-product x E))	; iloczyn
        ((and						; lista z jedną zmienną
            (equalp (length E) 1)
            (equalp (car E) x)
        ) 1)
        ((null E) 0)					; pusta lista (NIL)
        (t (print "Error: cannot parse expression.") (print E))))

(defun diff-sum (x E)
    "Różniczkuje sumę wyrażeń, tzn.: d (E1 + E2) = d E1 + d E2."
    (cons '+
        (map
            'list
            (lambda (expr) (d x expr))
            (cdr E))))

(defun diff-product (x E)
    "Różniczkuje iloczyn wyrażeń, tzn.: d E1 E2 = E1 d E2 + E2 d E1."
    (let(
        (E1 (car (cdr E)))				; pierwszy element iloczynu
        (E2 (cons '* (cdr (cdr E)))))			; (* pozostałe elementy iloczynu)
        (if (equalp (length E2) 2)
            (setf E2 (cdr E2)))
    (setf dE1 (d x E1))					; d E1
    (setf dE2 (d x E2))					; d E2
    (
        cons '+
        (list
            (cons '* (list E1 dE2))
            (cons '* (list E2 dE1))))))

;------------------------------------------------------------------------------
; Funkcje upraszczające zadane wyrażenie.
;------------------------------------------------------------------------------

(defun make-sum (x)
    "Tworzy sumę."
    (cons '+ x))

(defun make-product (x)
    "Tworzy iloczyn."
    (cons '* x))

(defun sum? (E)
    "Sprawdzenie czy wyrażenie jest sumą."
    (and (listp E) (equalp '+ (car E))))

(defun product? (E)
    "Sprawdzenie czy wyrażenie jest iloczynem."
    (and (listp E) (equalp '* (car E))))

(defun simplify (E)
    "Główna funkcja upraszczające wyrażenie."
    (cond
        ((sum? E) (simplify-sum E))
        ((product? E) (simplify-product E))
        (t E)))

(defun simplify-sum (E)
    "Funkcja upraszczające sumę."
    (simpl #'sum? #'make-sum 0 E))

(defun simplify-product (E)
    "Funkcja upraszczające iloczyn."
    (simpl #'product? #'make-product 1 E))

(defun remove-identity (E ident)
    "Usuwa wszystkie elementy neutralne operacji (0 dla sumy, 1 dla iloczynu)."
    (remove ident E))

(defun simpl (isit? addop ident E)
    "Wykonuje szereg uproszczeń."
    (let*
        (
          (parts (cdr E))				; Składniki operacji.
          (sparts (mapcar #'simplify parts))		; Uproszczone składniki operacji.
          (fparts (flat isit? sparts))			; Zamienione (* x (* y z)) --> (* x y z).
          (zout (replace-zero fparts))			; Zamienione (* ... 0 ...) --> 0.
          (unid (remove-identity zout ident))		; Usuń elementy neutralne operacji.
        )
        (proper addop ident unid)))			; Dodaje znak operacji do uproszczonego wyrażenia

; Funkcja rekurencyjna (P.B.)

(defun flat (isit args)
    "Zamienia np. (+ x y (+ z w) (+ q 4) g) --> (+ x y z w q 4 g)."
    (cond
        ((null args) ())				; Pusta lista.
        ;((not (pair? args)) (list args))
        ((funcall isit (car args))			; Operator pierwszego elementu taki sam jak aktualny, więc łączymy.
            (append (flat isit (cdar args)) (flat isit (cdr args)))
        )
        (t (cons (car args) (flat isit (cdr args))))))	; Operator pierwszego elementu inny niż aktualny, więc rekurencja dla kolejnych.

(defun proper (addop ident args)
    "Dodaje prefixowy znak operacji jeśli lista nie jest pusta lub 1-elementowa."
    (cond
        ((null args) ident)				; () --> 0 lub 1.
        ((null (cdr args)) (car args))			; (x) --> x
        (t (funcall addop args))))			; (x y z) --> (+/* x y z)

(defun is-zero-mult? (E)
    "Sprawdza czy iloczyn zawiera 0, żeby zamianić wyrażenie na 0."
    (and
        (product? E)
        (some (lambda (item) (equal 0 item)) E)
    ))

(defun replace-zero (expr)
    (if (null expr) ()
        (cons (if (is-zero-mult? (car expr))		; Jeśli pierwszy element list zawiera zero, to zastąp pojedynczym zerem.
                0
                (car expr))				; Wpp nic nie zmieniaj.
            (replace-zero (cdr expr)))))		; Wołaj rekurencyjnie dla reszty listy.

;------------------------------------------------------------------------------
; Przykłady.
;------------------------------------------------------------------------------

; Przykład usuwania zer:
;  (simplify '(+ (* 80 8 0) x (* 9 0 8)))
;
; Przykład usuwania zbędnych nawiasów:
;  (simplify '(+ (+ 80 8 0) x (+ 9 (+ 0 8))))
