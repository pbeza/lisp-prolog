%
% Facts about family tree.
%

% TODO Add family tree basen on given primitives: parent, man, woman, spouse.
% TODO Manually draw genealogy tree to easily figure out who is who.

parent(chester, irvin).
parent(chester, clarence).
parent(chester, mildred).
parent(irvin, ron).
parent(irvin, ken).
parent(clarence, shirley).
parent(clarence, sharon).
parent(clarence, charlie).
parent(mildred, mary).

man(chester).
woman(mildred).
man(irvin).
woman(shirley).
man(clarence).
woman(sharon).
man(ron).
woman(mary).
man(ken).
man(charlie).

%
% Rules.
%

% X is father of Y.
father(X, Y) :-
  parent(X, Y),
  man(X).

% X is mother of Y.
mother(X, Y) :-
  parent(X, Y),
  woman(X).

% X is son of Y.
son(X, Y) :-
  parent(Y, X),
  man(X).

% X is daughter of Y.
daughter(X, Y) :-
  parent(Y, X),
  woman(X).

% X is grandparent of Y.
grandparent(X, Y) :-
  parent(X, Z),
  parent(Z, Y).

% X is grandfather of Y.
grandfather(X, Y) :-
  father(X, Z),
  parent(Z, Y).

% X is grandmother of Y.
grandmother(X, Y) :-
  mother(X, Z),
  parent(Z, Y).

% X and Y are siblings.
sibling(X, Y) :-
  parent(Z, X),
  parent(Z, Y),
  X \= Y.

% X is sister of Y.
sister(X, Y) :-
  sibling(X, Y),
  female(X).

% X is brother of Y.
brother(X, Y) :-
  sibling(X, Y),
  male(X).

% X and Y are brothers.
brothers(X, Y) :-
  sibling(X, Y),
  man(X),
  man(Y).

% X and Y are sisters.
sisters(X, Y) :-
  sibling(X, Y),
  woman(X),
  woman(Y).

% X is aunt of Y.
aunt(X, Y) :-
  woman(X),
  sibling(X, Z),
  parent(Z, Y).

% X is aunt of Y.
aunt(X, Y) :-
  woman(X),
  spouse(X, W),
  sibling(W, Z),
  parent(Z, Y).

% X is uncle of Y.
uncle(X, Y) :-
  man(X),
  sibling(X, Z),
  parent(Z, Y).

% X is uncle of Y.
uncle(X, Y) :-
  man(X),
  spouse(X, W),
  sibling(W, Z),
  parent(Z, Y).
