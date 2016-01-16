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

% Who is man?
man(chester).
man(irvin).
man(clarence).
man(ron).
man(ken).
man(charlie).

% Who is woman?
woman(mildred).
woman(shirley).
woman(sharon).
woman(mary).
woman(chester_spouse).
woman(irvin_spouse).
woman(clarence_spouse).

% Who married who?
spouses(chester, chester_spouse).
spouses(irvin, irvin_spouse).
spouses(clarence, clarence_spouse).

% When who was born?
born(chester, 1940).
born(irvin, 1970).
born(clarence, 1975).
born(mildred, 1969).
born(ron, 1995).
born(ken, 1996).

% When who died?
death(chester, 1990).
death(irvin, 2000).
death(clarence, 2010).

% Constants.
current_year(2016).
adult_years(18).

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
  parent(X, Z),
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

% Does X is dead?
is_dead(X) :-
  death(X, _).

% Does X is not dead (yet).
is_alive(X) :-
  not(is_dead(X)).

% X had Y years old when died.
death_age(X, Y) :-
  is_dead(X),
  born(X, U),
  death(X, W),
  Y is W-U.

% X is alive and has Y years old.
alive_age(X, Y) :-
  is_alive(X),
  born(X, U),
  current_year(C),
  Y is C-U.

% X is the oldest alive person.
alive_oldest(X) :-
  alive_age(X, Y),
  not(alive_age(_, Z), Z>Y).

% X is adult. Dead people are NOT adult.
adult(X) :-
  alive_age(X, Y),
  adult_years(Z),
  Y>Z.

% X is adult without spouse.
adult_without_spouse(X) :-
  adult(X),
  not(spouses(X, _)).
