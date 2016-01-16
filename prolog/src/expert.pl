%
% Facts about family tree.
%

% TODO Add family tree basen on given primitives: parent, man, woman, spouse.
% TODO Manually draw genealogy tree to easily figure out who is who.

% chester is irvin's parent.
parent(chester, irvin).
parent(chester, clarence).
parent(chester, mildred).
parent(irvin, ron).
parent(irvin, ken).
parent(clarence, shirley).
parent(clarence, sharon).
parent(clarence, charlie).
parent(mildred, mary).
parent(ron, yoda).
parent(yoda, something).

% chester is man.
man(chester).
man(irvin).
man(clarence).
man(ron).
man(ken).
man(charlie).

% mildred is woman.
woman(mildred).
woman(shirley).
woman(sharon).
woman(mary).

% chester was born in 1940.
born(chester, 1940).
born(irvin, 1970).
born(clarence, 1975).

% chester died in 1990.
death(chester, 1990).
death(irvin, 2000).
death(clarence, 2010).

%
% Rules.
%

% X is Y's father.
father(X, Y) :-
  parent(X, Y),
  man(X).

% X is Y's mother.
mother(X, Y) :-
  parent(X, Y),
  woman(X).

% X is Y's son.
son(X, Y) :-
  parent(X, Y),
  man(X).

% X is Y's daughter.
daughter(X, Y) :-
  parent(Y, X),
  woman(X).

% X is Y's grandparent.
grandparent(X, Y) :-
  parent(X, Z),
  parent(Z, Y).

% X is Y's grandfather.
grandfather(X, Y) :-
  father(X, Z),
  parent(Z, Y).

% X is Y's grandmother.
grandmother(X, Y) :-
  mother(X, Z),
  parent(Z, Y).

% X and Y are siblings.
sibling(X, Y) :-
  parent(Z, X),
  parent(Z, Y),
  X \= Y.

% X is Y's sister.
sister(X, Y) :-
  sibling(X, Y),
  female(X).

% X is Y's brother.
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

% X is Y's aunt.
aunt(X, Y) :-
  woman(X),
  sibling(X, Z),
  parent(Z, Y).

% X is Y's aunt.
aunt(X, Y) :-
  woman(X),
  spouse(X, W),
  sibling(W, Z),
  parent(Z, Y).

% X is Y's uncle.
uncle(X, Y) :-
  man(X),
  sibling(X, Z),
  parent(Z, Y).

% X is Y's uncle.
uncle(X, Y) :-
  man(X),
  spouse(X, W),
  sibling(W, Z),
  parent(Z, Y).

% X is Y's ancestor.
ancestor(X, Y) :-
  parent(X, Y).

ancestor(X, Y) :-
  ancestor(X, Z),
  ancestor(Z, Y).

% X is Y's and Z's common ancestor.
common_ancestor(X, Y, Z) :-
  ancestor(X, Y),
  ancestor(X, Z).

% X has ancestor without sibling.
has_ancestor_without_sibling(X) :-
  ancestor(Y, X),
  not(sibling(Y, _)).

% X has dead ancestor.
has_dead_ancestor(X) :-
  ancestor(Y, X),
  death(Y, _).

% Does exist person who has X, Y ancestors?
exist_person_with_XY_ancestors(X, Y) :-
  ancestor(X, U),
  ancestor(Y, U).
