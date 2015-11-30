symdiff  :-
   write('Wpisz wyrazenie albo "q": '),
   read(Exp),
   process(Exp).

process(q) :- !.

/*
 * 1. Zapytaj o wyrażenie i zmienną do różniczkowania.
 * 2. Zróżniczkuj wyrażenie po tej zmiennej.
 * 3. Uprość wynik i wypisz.
 * 4. Zapętl.
 */
process(Exp) :-
   write('Różniczkuj po zmiennej: '),
   read(Wrt),
   write('Która pochodna? (> 0): '),
   read(Nth),
   compute(Exp, Wrt, Sdiff),
   /*compute(Exp, Wrt, Nth, Sdiff),*/
   write('Pochodna '), write(Wrt),
   write(' to '),
   write(Sdiff), nl, nl,
   symdiff.

compute(Exp, Wrt, Sdiff) :-
   diff(Exp, Wrt, Diff),
   simplify(Diff, Sdiff).

/*
compute(Exp, Wrt, Nth, Ndiff) :-
   diff(Exp, Wrt, Diff),
   simplify(Diff, Sdiff),
   Nthdec is Nth - 1,
   Ndiff is Sdiff,
   Nthdec > 0,
   compute(Sdiff, Wrt, Nthdec, Ndiff).
*/

/*
 * Różniczkowanie x po x to zawsze 1.
 */
diff(X, X, 1) :- !.

/*
 * Stała (nie będąca x) różniczkowana po x == 0.
 */
diff(C, _X, 0) :- atomic(C).

/*
 * d/dx U+V, gdzie U i V dowolnei.
 */
diff(U+V, X, A+B) :-
   diff(U, X, A),
   diff(V, X, B).

/*
 * d/dx U-V, gdzie U i V dowolne.
 */
diff(U-V, X, A-B) :-
   diff(U, X, A),
   diff(V, X, B).

/*
 * d/dx C*U, gdzie U dowolne, C - stała albo symbol inny niż X.
 */
diff(C*U, X, C*A) :-
   atomic(C),
   C \= X,
   diff(U, X, A), !.

/*
 * d/dx U*V, gdzie U i V dowolne nie "złapane" powyżej.
 */
diff(U*V, X, B*U+A*V) :-
   diff(U, X, A),
   diff(V, X, B).

/*
 * d/dx U/V, gdzie U i V dowolne.
 * TODO Można tu złapać dzielenie przez 0.
 */
diff(U/V, X, (A*V-B*U)/(V*V)) :-
   diff(U, X, A),
   diff(V, X, B).

/*
 * d/dx U^C, gdzie: U dowolne, C - stała albo symbol inny niż X.
 */
diff(U^C, X, C*A*U^(C-1)) :-
   atomic(C),
   C\=X,
   diff(U, X, A).

/*
 * J.w., ale C z minusem.
 */
diff(U^C, X, C*A*U^(C-1)) :-
   C = -(C1), atomic(C1),
   C1\=X,
   diff(U, X, A).

/*
 * Szczególny przypadek: x^x.
 */
diff(U^U, X, (log(U)+1)*U^U) :-
   diff(U, X, _A).

/*
 * Wybrane funkcje jednoargumentowe, dla dowolnego argumentu.
 */
diff(sin(W), X, Z*cos(W)) :-
   diff(W, X, Z).

diff(cos(W), X, -(Z*sin(W))) :-
   diff(W, X, Z).

diff(exp(W), X, Z*exp(W)) :-
   diff(W, X, Z).

diff(log(W), X, Z/W) :-
   diff(W, X, Z).

/* Upraszczanie wyrażeń */
/* Tylko sąsiednich...  */

/*
 * Atom jest już uproszczony.
 */
simplify(X, X) :-
   atomic(X), !.

/*
 * Odrzucamy dodane zero z obu stron.
 */
simplify(X+0, Y) :-
   simplify(X, Y).

simplify(0+X, Y) :-
   simplify( X, Y ).

/*
 * J.w. dla odejmowania.
 */
simplify(X-0, Y) :-
   simplify(X, Y).

simplify(0-X, -(Y)) :-
   simplify(X, Y).

/*
 * Suma stałych.
 */
simplify(A+B, C) :-
   numeric(A),
   numeric(B),
   C is A+B.

/*
 * x-x to 0 dla dowolnego x (w tym wyrażeń).
 */
simplify(A-A, 0).

/*
 * Różnica stałych.
 */
simplify( A-B, C ) :-
   numeric(A),
   numeric(B),
   C is A-B.

/*
 * Uproszczenia mnożenia i dzielenia.
 */
simplify(_X*0, 0).
simplify(0*_X, 0).

/*
 * Dzielenie zera (TODO: tu można by wywalić błąd przy dzieleniu przez 0).
 */
simplify(0/_X, 0).

simplify(X*1, X).
simplify(1*X, X).
simplify(X/1, X).
simplify(X/X, 1) :- !.
simplify(X^1, X) :- !.
simplify(_X^0, 1) :- !.

/*
 * Zamiana iloczynu na potęgowanie.
 */
simplify(X*X, X^2) :- !.

/*
 * "Zwijanie" do istniejącej potęgi.
 */
simplify(X*X^A, Y) :-
   simplify(X^(A+1), Y), !.

simplify(X^A*X, Y) :-
   simplify(X^(A+1), Y), !.

/*
 * Mnożenie stałych.
 */
simplify(A*B, X) :-
   numeric(A),
   numeric(B),
   X is A*B.

/*
 * A*X+B*X = (A+B)*X (chyba że A == X albo B == X).
 */
simplify(A*X+B*X, Z) :-
   A\=X, B\=X,
   simplify((A+B)*X, Z).

/*
 * Wzór skróconego mnożenia: (A+B)*(A-B) = A^2 + B^2.
 */
simplify((A+B)*(A-B), X^2-Y^2) :-
   simplify(A, X),
   simplify(B, Y).

/*
 * X^A/X^B = X^(A-B)
 */
simplify(X^A/X^B, X^C) :-
   numeric(A), numeric(B),
   C is A-B.

/*
 * Dzielenie stałych.
 */
simplify(A/B, X) :-
   numeric(A),
   numeric(B),
   X is A/B.

/*
 * Potęgowanie stałych.
 */
simplify(A^B, X) :-
   numeric(A),
   numeric(B),
   X is A^B.

/*
 * Suma wyrażeń.
 * Warunek W \== Y zapewnia że ten predykat nie "zajmie" podstawienia nic przy tym nie robiąc.
 */
simplify(W+X, Q) :-
   simplify(W, Y),
   simplify(X, Z),
   (W \== Y ; X \== Z),
   simplify(Y+Z, Q).

/*
 * Różnica wyrażeń.
 */
simplify(W-X, Q) :-
   simplify(W, Y),
   simplify(X, Z),
   (W \== Y ; X \== Z),
   simplify(Y-Z, Q).

/*
 * Iloczyn wyrażeń.
 */
simplify(W*X, Q) :-
   simplify(W, Y),
   simplify(X, Z),
   (W \== Y  ; X \== Z),
   simplify(Y*Z, Q).

/*
 * Iloraz wyrażeń.
 */
simplify(A/B, C) :-
   simplify(A, X),
   simplify(B, Y),
   (A \== X ; B \== Y),
   simplify(X/Y, C).

/*
 * Potęgowanie wyrażeń.
 */
simplify(X^A, C) :-
   simplify(A, B),
   A \== B,
   simplify(X^B, C).

/*
 * Zabezpieczenie żeby "dziwne" wyrażenia nie spowodowały błędów.
 */
simplify(X, X).

/*
 * Atom jest liczbą kiedy jest liczbą całkowitą lub zmiennoprzecinkową.
 */
numeric(A) :- integer(A).
numeric(A) :- float(A).
