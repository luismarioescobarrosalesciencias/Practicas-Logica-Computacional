% Practica 5

% 1. elemento. Función que determina si un elemento pertenece a una lista
elemento(X,[X|_]).
elemento(X,[_|XS]) :- elemento(X,XS).

% 2. suma. Función que suma todos los elementos de una lista
suma([], 0).
suma([X|XS], S) :- suma(XS, R), S is X + R.

% 3. palindromo. Funcion que verifica si una lista es palindromo o no.
palindromo([]).
palindromo([_]).
palindromo(L) :- reversa(L,L).

% reversa. Devuelve la reversa de una lista
reversa([],[]).
reversa([X|XS], Reversa):- reversa(XS, ReversaXS), conc(ReversaXS, X, Reversa).

% conc. Concatena dos listas.
conc([],X,[X]).
conc([X|D],B,[X|E]) :- conc(D,B,E).

% comparacion. Compara dos listas y determina si son iguales o no.
%comparacion([],[]).
%comparacion([X],[Y]) :- X =:= Y.
%comparacion([X|XS],[Y|YS]) :- X =:= Y, comparacion(XS,YS).

% 4. ocurrir. Funcion que devuelve el numero de apariciones de un elemento en
%             una lista.
ocurrir([],_,0).
ocurrir([X],X,1).
ocurrir([Y],X,0).
ocurrir([X|XS],X,NumVeces) :- ocurrir(XS,X,S), NumVeces is 1 + S.
ocurrir([_|YS],X,NumVeces) :- ocurrir(YS,X,S), NumVeces is S.

app([], List, List).
app([Head|Tail], List, [Head|Rest]) :- app(Tail, List, Rest).

my_append([], Cs, Cs).
my_append([A|As],Bs,[A|Cs]):- my_append(As, Bs, Cs).

inorden(empty,[]).
inorden(tree(X, empty, empty), [X]).
inorden(tree(Y, L, R),S) :- inorden(L,InoL), inorden(R,InoR), conc(InoL,Y,M), conc(M,InoR,S).
