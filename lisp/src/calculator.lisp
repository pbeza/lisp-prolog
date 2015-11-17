; Symbolic calculator.

;-------------------------------------
; INFIX -> PREFIX
;-------------------------------------

; Infix to prefix translator

(defvar *separators* (list '+ '- '* '/) "Default operators for the math macro") 

(defun remove-brackets (lst)
    "Reduces lists with just one item to the item itself"
    (if (or (not (consp lst))
            (not (null (cdr lst))))
        lst
        (remove-brackets (car lst))))

(defun separate-list (lst separator test)
    "Returns list of sub-sequences defined by separator"
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
            (do () ((null lst) result) ; end condition and return value
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
    "Apply separate-list on all sublists"
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
    "Converts an infix expression to prefix"
    (let ((result infix-expr))
    (dolist (sep separators)
        (setf result (separate-tree result sep test)))
    (remove-brackets result)))

(defun calc (infix-expr)
    "Calculate result of given expression"
    (eval (infix->prefix infix-expr *separators*)))

;-------------------------------------
; FACTORIAL
;-------------------------------------

; Factorial - recursive macro

(defmacro macro-factorial (n)
    "Macro factorial"
    (if (= 0 n)
        '1
        (let ((m (1- n)))
            `(* ,n (macro-factorial ,m)))))

; Factorial - recursive function

(defun factorial (n)
    "Function factorial"
    (case n
        (0 1)
        (10 (macro-factorial 10))
        (otherwise (* n (factorial (1- n))))))

;-------------------------------------
; TAYLOR SERIES FOR SINUS
;-------------------------------------

; Taylor series for sinus - recursive function

(defun term (n radians)
    "n-th term of Taylor sine series"
    (*
        (/
            (expt radians (+ (* 2 n) 1))
            (factorial (+ (* 2 n) 1))
        )
        (expt -1 n)
    )
)

(defun reduce-angle (x)
    "Reduce angle to [-pi, pi] range"
    (- x (* (round (/ x (* 2 pi))) 2 pi)))

(defun sine-iter (radians n val)
    "Recursive function for Taylor series sum"
    (if (>= n 16)
        val
        (sine-iter radians (+ n 1) (+ val (term (+ n 1) radians)))))

(defun taylor-sine (radians)
    "First call for Taylor series"
    (sine-iter (reduce-angle radians) -1 0))

; Taylor series for sinus - recursive macro

(defmacro macro-sine-iter (radians n val)
    "Recursive macro for Taylor series sum"
    (if (>= n 16)
        val
        `(macro-sine-iter ,radians ,(+ n 1) ,(+ val (term (+ n 1) radians)))))

;(defun macro-taylor-sine (radians)
;    "First call for Taylor series"
;    (macro-sine-iter radians -1 0))

;-------------------------------------
; FIBONACCI
;-------------------------------------

; Fibonacci(10)
;(do ((n 0 (1+ n))  ;declares n, initially 0, n+1 each subsequent iteration)
;     (cur 0 next)   ;declares cur, initially 0, then old value of next
;     (next 1 (+ cur next))) ;declares next, initially 1, then the sum of (the old) cur and next
;    ((= 10 n) ;end condition (ends when n = 10)
;     cur)    ; return value
;  ;empty body
;  )
