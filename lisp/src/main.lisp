;------------------------------------------------------------------------------
; Konwersja postaci infixowej wyrażenia na prefixową.
;------------------------------------------------------------------------------


(defvar *separators* (list '+ '- '* '/) "Domyślnie rozpoznawane operatory")

; Funkcja rekurencyjna (P.B.)

(defun remove-brackets (lst)
    "Redukuje listę z jednym elementem do tego elementu."
    (if (or (not (consp lst))
            (not (null (cdr lst))))
      lst
      (remove-brackets (car lst))))

(defun separate-list (lst separator test)
    "Zwraca listę podlist poprzedzonych separatoremi, wyrzucając separatory infixowe."
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
            (do () ((null lst) result) ; Warunek końcowy i zwracana wartość.
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

; Funkcja rekurencyjna (P.B.)

(defun separate-tree (lst separator test)
    "Wywołaj separate-list na wszystkich podlistach zaczynając od najbardziej zagnieżdżonych."
    (if (or (not (consp lst)) (eql (first lst) 'quote))
        lst
        (progn
            (setf lst (mapcar #'(lambda (x)
                (if (not (consp x))
                    x
                    (separate-tree x separator test) ; Znaleziono podlistę.
                )) lst)
            )
            ;(print 'separate-tree) (print lst)
            (if (not (find separator (rest lst)))
                lst
                (separate-list lst separator test)  ; "Wyjmij" znak przed wyrażenie.
            )
        )
    )
)

(defun infix->prefix (infix-expr separators &key (test #'eql))
    "Zamienia postać infixową wyrażenia na prefixową."
    (let ((result infix-expr))
    (dolist (sep separators)
        (setf result (separate-tree result sep test))
        ;(print 'infix->prefix) (print result)
    )
    (remove-brackets result)))

(defmacro !! (body)
    "Konwersja postaci infix na prefix z domyślnymi separatorami."
    `(infix->prefix ,body *separators*))


;------------------------------------------------------------------------------
; Operacje matematyczne.
;------------------------------------------------------------------------------


; Makro rekurencyjne (J.D.)

(defmacro macro-factorial (n)
    "Silnia - makro."
    (cond
        ((not (numberp n)) `(factorial ,n))
        ((= 0 n) '1)
        (t (let ((m (1- n)))
            `(* ,n (macro-factorial ,m))))))

; Funkcja rekurencyjna (J.D.)

(defun factorial (n)
    "Silnia - funkcja."
    (cond
        ((not (numberp n)) `(factorial ,n))
        ((>= 1 n) 1)
        (t (let ((m (1- n)))
            (* n (factorial m))))))

; Funkcja rekurencyjna (J.D.)

(defun calculable (expr)
    (cond
        ((numberp expr) t)
        ((consp expr) (and (calculable (first expr)) (calculable (rest expr))))
        (t nil)))

(defun symbolic-reduce (rsym rfun expr initial)
    (let* (
        (partitioned (partition #'numberp expr))
        (numbers (first partitioned))
        (symbols (second partitioned))
        (reduced (reduce-inner rfun numbers initial))
    )
    ;(format t "~&~%~a~&~%~a" expr partitioned)
    (if (null symbols)
        reduced
        (append `(,rsym ,reduced) symbols))))

; Funkcja rekurencyjna (D.J.)

(defun reduce-inner (rfun numbers ret)
    (setf item (first numbers))
    (if (null item)
        ret
        (reduce-inner rfun (rest numbers) (funcall rfun item ret))))

(defun partition (predicate expr)
    "Dzieli listę na liczby i całą resztę (symbole, funkcje, wyrażenia)."
    (partition-inner predicate expr nil nil))

; Funkcja rekurencyjna (D.J.)

(defun partition-inner (predicate expr symbols values)
    (setf item (first expr))
    (cond
        ((null item) (list values symbols))
        ((funcall predicate item) (partition-inner predicate (rest expr) symbols (append values (list item))))
        (t (partition-inner predicate (rest expr) (append symbols (list item)) values))))

(defun partition-innerr (predicate expr)
    (reduce (lambda (a b)
                (if (funcall predicate a)
                        (push a (first b))
                    (push a (second b)))
                b)
            expr
            :initial-value (list nil nil)
            :from-end t))

(defun plus (&rest expr)
    (symbolic-reduce '+ #'+ expr 0))

;(defun minus (&rest expr)
;    (plus (cons (first expr) (apply #'multiply (cons -1 expr)))))

(defun multiply (&rest expr)
    (setf mult (symbolic-reduce '* #'* expr 1))
    (cond
        ((not (consp mult)) mult)
        ((>= 1 (length mult)) mult)
        ((equal (nth 1 mult) 0) 0)
        (t mult)))

;(defun divide (&rest args)
;    (apply '/ args))


;------------------------------------------------------------------------------
; Różniczkowanie symboliczne.
;------------------------------------------------------------------------------


; Funkcja rekurencyjna (P.B.)

(defun d (x E)
    "Różniczkuje zadane wyrażenie E składające się ze zmiennej x."
    (setf E (remove-brackets E))
    (cond
        ((integerp E) 0)                            ; stała liczbowa
        ((equalp E x) 1)                            ; zmienna
        ((equalp (length (write-to-string E)) 1) E) ; stała "literowa"
        ((equalp '+ (car E)) (diff-sum x E))        ; suma
        ((equalp '* (car E)) (diff-product x E))    ; iloczyn
        ((and                                       ; lista z jedną zmienną
            (equalp (length E) 1)
            (equalp (car E) x)
        ) 1)
        ((null E) 0)                                ; pusta lista (NIL)
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
        (E1 (car (cdr E)))                          ; pierwszy element iloczynu
        (E2 (cons '* (cdr (cdr E)))))               ; (* pozostałe elementy iloczynu)
        (when (equalp (length E2) 2)
            (setf E2 (cdr E2))
            (if (equalp (length E2) 1)
                (setf E2 (car E2))))
    (setf dE1 (d x E1))                             ; d E1
    (setf dE2 (d x E2))                             ; d E2
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
          (parts (cdr E))                           ; Składniki operacji.
          (sparts (mapcar #'simplify parts))        ; Uproszczone składniki operacji.
          (fparts (flat isit? sparts))              ; Zamienione (* x (* y z)) --> (* x y z).
          (zout (replace-zero fparts))              ; Zamienione (* ... 0 ...) --> 0.
          (unid (remove-identity zout ident))       ; Usuń elementy neutralne operacji.
        )
        (proper addop ident unid)))                 ; Dodaje znak operacji do uproszczonego wyrażenia

; Funkcja rekurencyjna (P.B.)

(defun flat (isit args)
    "Zamienia np. (+ x y (+ z w) (+ q 4) g) --> (+ x y z w q 4 g)."
    (cond
        ((null args) ())                            ; Pusta lista.
        ;((not (pair? args)) (list args))
        ((funcall isit (car args))                  ; Operator pierwszego elementu
                                                    ; taki sam jak aktualny, więc łączymy.
            (append (flat isit (cdar args)) (flat isit (cdr args)))
        )
        (t (cons (car args) (flat isit (cdr args))))))
                                                    ; Operator pierwszego elementu inny
                                                    ; niż aktualny, więc rekurencja dla kolejnych.

(defun proper (addop ident args)
    "Dodaje prefixowy znak operacji jeśli lista nie jest pusta lub 1-elementowa."
    (cond
        ((null args) ident)                         ; () --> 0 lub 1.
        ((null (cdr args)) (car args))              ; (x) --> x
        (t (funcall addop args))))                  ; (x y z) --> (+/* x y z)

(defun is-zero-mult? (E)
    "Sprawdza czy iloczyn zawiera 0, żeby zamianić wyrażenie na 0."
    (and
        (product? E)
        (some (lambda (item) (equal 0 item)) E)
    ))

(defun replace-zero (expr)
    (if (null expr) ()
        (cons (if (is-zero-mult? (car expr))        ; Jeśli pierwszy element list zawiera
                                                    ; zero, to zastąp pojedynczym zerem.
                0
                (car expr))                         ; Wpp nic nie zmieniaj.
            (replace-zero (cdr expr)))))            ; Wołaj rekurencyjnie dla reszty listy.

;------------------------------------------------------------------------------
; Przykłady różniczkowania i uproszczeń wyrażeń.
;------------------------------------------------------------------------------

;------
; Różniczkowanie funkcji x^3:
;   (d 'x '(* x x x))
;
; Wynik (bez uproszczenia):
;   (+ (* X (+ (* X 1) (* X 1))) (* (* X X) 1))
;------
; Upraszczanie wyniku otrzymanego z różniczkowania f-cji x^3:
;   (simplify '(+ (* X (+ (* X 1) (* (X) 1))) (* (* X X) 1)))
;
; Wynik:
;   (+ (* X (+ X X)) (* X X))
;------
; Przykład usuwania zer:
;   (simplify '(+ (* 80 8 0) x (* 9 0 8)))
;
; Wynik:
;   X
;------
; Przykład usuwania zbędnych nawiasów:
;   (simplify '(+ (+ 80 8 0) x (+ 9 (+ 0 8))))
;
; Wynik:
;   (+ 80 8 X 9 8)
;------

;------------------------------------------------------------------------------
; Podstawowa funkcjonalność kalkulatora.
;------------------------------------------------------------------------------



(defun expand-exp (expr)
    "Rozwija funkcję wykładniczą. Np. e^(x+2) -> e^x*e^2"
    (cond
        ((null (first expr)) nil)
        ((and (consp (first expr)) (equal 'exp (first (first expr)))))
        ((consp (first expr)) (cons (expand (first expr)) (expand (rest expr))))
        (t (cons (first expr) (expand (rest expr))))
    ))

(defun rename-one (item)
    "Mapowanie operacja -> funkcja."
    (case item
        ('quote nil)
        ('+ #'plus)
        ;('- #'minus)
        ('* #'multiply)
        ;('/ #'divide)
        ('factorial #'factorial)
        ('d #'d)
        (t item)))

; Funkcja rekurencyjna (J.D.)

(defun rename-functions (expr)
    "Zamienia +, -, * i / na równoważne funkcje."
    (cond
        ((null expr) nil)
        ;((equal (first expr) 'quote) (first (rename-functions (rest expr))))
        ((consp (first expr)) (cons (rename-functions (first expr)) (rename-functions (rest expr))))
        (t (cons (rename-one (first expr)) (rename-functions (rest expr))))
    ))

; Funkcja rekurencyjna (J.D.)

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
    (postcalc (simplify (precalc (simplify expr)))))
    ;(postcalc (precalc expr)))

(defun calc-print (expr)
    (setf inlined (!! expr))
    (setf calced (calc inlined))
    (format t "~&~%~a~&~%~a~&~%~a~&~%" expr inlined calced)
    (calc inlined))

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
(defvar *ex* '(3 * 2 * (factorial 5 + (0 * x * (0 * x))) + 2 + x + (factorial x) + (d x (* x x x))))

;(trace postcalc)
;(trace precalc)
;(trace multiply)
;(trace plus)
;(trace symbolic-reduce)
;(trace partition-inner)

(calc-print *ex*)
;(main-loop)
