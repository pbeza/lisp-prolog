%
% Facts about family tree.
%

:- dynamic(add_man/4).
:- dynamic(add_man/5).
:- dynamic(add_man/6).
:- dynamic(add_woman/4).
:- dynamic(add_woman/5).
:- dynamic(add_woman/6).
:- dynamic(parent/2).
:- dynamic(man/1).
:- dynamic(woman/1).
:- dynamic(spouses/2).
:- dynamic(born/2).
:- dynamic(death/2).
:- dynamic(current_year/1).
:- dynamic(adult_years/1).

% TODO Manually draw genealogy tree to easily figure out who is who.

% Who is whose parent?

parent(chester, irvin).
parent(chester, clarence).
parent(chester, mildred).
parent(chester_spouse, irvin).
parent(chester_spouse, clarence).
parent(chester_spouse, mildred).
parent(irvin, ron).
parent(irvin, ken).
parent(irvin_spouse, ron).
parent(irvin_spouse, ken).
parent(clarence, shirley).
parent(clarence, sharon).
parent(clarence, charlie).
parent(clarence_spouse, shirley).
parent(clarence_spouse, sharon).
parent(clarence_spouse, charlie).
parent(mildred, mary).
parent(mildred_spouse, mary).
parent(ron, yoda).
parent(ron_spouse, yoda).
parent(yoda, something).
parent(yoda_spouse, something).

% Who is man?

man(chester).
man(irvin).
man(clarence).
man(ron).
man(ken).
man(charlie).
man(mildred_spouse).
man(shirley_spouse).
man(yoda).

% Who is woman?

woman(mildred).
woman(shirley).
woman(sharon).
woman(mary).
woman(chester_spouse).
woman(irvin_spouse).
woman(clarence_spouse).
woman(ron_spouse).
woman(yoda_spouse).

% Who married who?

spouses(chester, chester_spouse).
spouses(irvin, irvin_spouse).
spouses(clarence, clarence_spouse).
spouses(mildred, mildred_spouse).
spouses(ron, ron_spouse).
spouses(yoda, yoda_spouse).

% When who was born?

born(chester, 1940).
born(chester_spouse, 1945).
born(irvin, 1970).
born(irvin_spouse, 1976).
born(clarence, 1975).
born(clarence_spouse, 1976).
born(mildred, 1969).
born(mildred_spouse, 1961).
born(ron, 1995).
born(ron_spouse, 1995).
born(ken, 1996).

% When who died?

death(chester, 1990).
death(chester_spouse, 2015).
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
  spouses(X, W),
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
  spouses(X, W),
  sibling(W, Z),
  parent(Z, Y).

% X is Y's ancestor with degree of relationship equal 1.
ancestor(X, Y, 1) :-
  parent(X, Y).

% X is Y's ancestor with degree of relationship equal W.
ancestor(X, Y, W) :-
  parent(X, Z),
  ancestor(Z, Y, U),
  W is U+1.

% X is Y's and Z's common ancestor.
common_ancestor(X, Y, Z) :-
  ancestor(X, Y, _),
  ancestor(X, Z, _).

% X has ancestor without sibling.
has_ancestor_without_sibling(X) :-
  ancestor(Y, X, _),
  \+ sibling(Y, _).

% X has dead ancestor.
has_dead_ancestor(X) :-
  ancestor(Y, X, _),
  death(Y, _).

% Does exist person who has X, Y ancestors?
exist_person_with_XY_ancestors(X, Y) :-
  ancestor(X, U, _),
  ancestor(Y, U, _).

% Is X dead?
is_dead(X) :-
  death(X, _).

% Is X alive?
% X is alive when X was born and X is not dead
is_alive(X) :-
  born(X, _),
  \+ is_dead(X).

% X was Y years old when died.
death_age(X, Y) :-
  is_dead(X),
  born(X, U),
  death(X, W),
  Y is W-U.

% X is alive and is Y years old.
alive_age(X, Y) :-
  is_alive(X),
  born(X, U),
  current_year(C),
  Y is C-U.

% X is the oldest alive person.
alive_oldest(X) :-
  alive_age(X, Y),
  \+ (alive_age(_, Z), Z > Y).

% X is alive adult.
alive_adult(X) :-
  alive_age(X, Y),
  adult_years(Z),
  Y>=Z.

% X was adult when died.
dead_adult(X) :-
  death_age(X, Y),
  adult_years(Z),
  Y>=Z.

% X is adult and has no spouse.
adult_without_spouse(X) :-
  alive_adult(X),
  \+ spouses(X, _).

% X and Y are part of the same living family. Affinity is excluded. So called consanguinity or bloodline.
part_of_the_same_living_family_without_affinity(X, Y) :-
  common_ancestor(Z, X, Y),
  is_alive(Z).

% X and Y are part of the same living family. Affinity is included.
part_of_the_same_family_with_affinity(X, Y) :-
  spouses(X, Y).

% X and Y are part of the same living family. Affinity is included.
part_of_the_same_family_with_affinity(X, Y) :-
  spouses(U, W),
  ancestor(U, X, _),
  ancestor(W, Y, _).

% X and Y are part of the same living family. Affinity is included.
% Depending on the family's definition it should be commented out or not.
part_of_the_same_family_with_affinity(X, Y) :-
  spouses(U, W),
  ancestor(X, U, _),
  ancestor(Y, W, _).

% X is descendant of Y with degree of relationship less or equal W.
nth_or_less_degree_ancestor(X, Y, W) :-
  ancestor(Y, X, Z),
  W>=Z.

add_person(NAME, PARENT1, PARENT2, BIRTH_YEAR) :-
  assert(parent(PARENT1, NAME)),
  assert(parent(PARENT2, NAME)),
  assert(born(NAME, BIRTH_YEAR)).

add_man(NAME, PARENT1, PARENT2, BIRTH_YEAR) :-
  add_person(NAME, PARENT1, PARENT2, BIRTH_YEAR),
  assert(man(NAME)).

add_woman(NAME, PARENT1, PARENT2, BIRTH_YEAR) :-
  add_person(NAME, PARENT1, PARENT2, BIRTH_YEAR),
  assert(woman(NAME)).

add_man(NAME, PARENT1, PARENT2, BIRTH_YEAR, SPOUSE) :-
  add_man(NAME, PARENT1, PARENT2, BIRTH_YEAR),
  assert(spouses(NAME, SPOUSE)).

add_woman(NAME, PARENT1, PARENT2, BIRTH_YEAR, SPOUSE) :-
  add_woman(NAME, PARENT1, PARENT2, BIRTH_YEAR),
  assert(spouses(NAME, SPOUSE)).

add_man(NAME, PARENT1, PARENT2, BIRTH_YEAR, SPOUSE, DEATH_YEAR) :-
  add_man(NAME, PARENT1, PARENT2, BIRTH_YEAR, SPOUSE),
  assert(death(DEATH_YEAR)).

add_woman(NAME, PARENT1, PARENT2, BIRTH_YEAR, SPOUSE, DEATH_YEAR) :-
  add_man(NAME, PARENT1, PARENT2, BIRTH_YEAR, SPOUSE),
  assert(death(DEATH_YEAR)).

menu :-
  write('***************'), nl,
  write('* 1. Add alive man without spouse.'), nl,
  write('* 2. Add alive woman without spouse.'), nl,
  write('* 3. Add alive man with spouse.'), nl,
  write('* 4. Add alive woman with spouse.'), nl,
  write('* 5. Add dead man with spouse.'), nl,
  write('* 6. Add dead woman with spouse.'), nl,
  write('Please, select option number.'), nl,
  read(CHOICE), nl,
  process(CHOICE, NAME),
  write(NAME),
  write(' successfully added.'), nl, nl.

process_(NAME, PARENT1, PARENT2, BIRTH_YEAR) :-
  write('Enter name:'), nl,
  read(NAME), nl,
  write('Name of the first parent:'), nl,
  read(PARENT1), nl,
  write('Name of the second parent:'), nl,
  read(PARENT2), nl,
  write('Birth year:'),nl,
  read(BIRTH_YEAR), nl.

process(1, NAME) :-
  process_(NAME, PARENT1, PARENT2, BIRTH_YEAR),
  add_man(NAME, PARENT1, PARENT2, BIRTH_YEAR).

process(2, NAME) :-
  process_(NAME, PARENT1, PARENT2, BIRTH_YEAR),
  add_woman(NAME, PARENT1, PARENT2, BIRTH_YEAR).

process(3, NAME) :-
  process(1, NAME),
  write('Name of the spouse:'), nl,
  read(SPOUSE_NAME), nl,
  assert(spouses(NAME, SPOUSE_NAME)).

process(4, NAME) :-
  process(2, NAME),
  write('Name of the spouse:'), nl,
  read(SPOUSE_NAME), nl,
  assert(spouses(NAME, SPOUSE_NAME)).

process(5, NAME) :-
  process_(NAME, PARENT1, PARENT2, BIRTH_YEAR),
  write('Name of the spouse:'), nl,
  read(SPOUSE_NAME), nl,
  write('Death year:'), nl,
  read(DEATH_YEAR), nl,
  add_man(NAME, PARENT1, PARENT2, BIRTH_YEAR, SPOUSE_NAME, DEATH_YEAR).

remove_person(NAME) :-
  retract(NAME).

remove_man(NAME) :-
  man(NAME),
  remove_person(NAME).

remove_woman(NAME) :-
  woman(NAME),
  remove_person(NAME).
