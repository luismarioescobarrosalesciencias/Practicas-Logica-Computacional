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

% Implementa cada una de las propiedades de relaciones de equivalencia sobre el mundo de los gatos
% Universo gatos 
gato(meow).
gato(pepe).
gato(nina).
gato(chucho).
gato(carla).

% Relacion reflexiva, definida para el universo de gatos
ser(X,X):-gato(X).

% Relacion simetrica 
amistad(meow,pepe).
amistad(nina,carla).
amistad(chucho,carla).
amistad(chucho,pepe).

amigos(X,Y) :- gato(Y),gato(X),amistad(X,Y);amistad(Y,X).

% Transitiva 
% "SI x es mas grande que y, y y mas grande que z entonces x mas grande que z"
grande(chucho,meow).
grande(chucho,pepe).
grande(meow,pepe).
grande(meow,carla).
grande(pepe,carla).
grande(carla,nina).

masgrande(X,Y):- gato(Y),gato(X),grande(X,Y).
masgrande(X,Y):- gato(Y),gato(X), grande(X,Z),masgrande(Z,Y).   % cerradura transitiva 

% Implementa los hechos que representen a cada integrante  las reglas familiares necesarias para poder preguntar porla relaci ́on ’abuelo’

hombre(autor).
mujer(esposa_autor).

esposo(autor,esposa_autor).
esposa(esposa_autor,autor).

esposo(X,Y):- hombre(X).
esposa(X,Y):- mujer(X).

hombre(padre_autor).
padre(padre_autor,autor).
hijo(autor,padre_autor).


padre(X,Y):- hombre(X),hijo(Y,X).

mujer(hijastra_autor).
madre(esposa_autor,hijastra_autor).
madre(X,Y):- mujer(X),hijo(Y,X).

hijo(hijastra_autor,autor).
padre(autor,hijastra_autor).


esposo(padre_autor,hijastra_autor).
esposa(hijastra_autor,padre_autor).

hijo(autor,hijastra_autor).
% yerno(padre_autor,autor).
% yerno(X,Y):-esposo(X,Z),hijo(Z,Y). % x yerno de "y"

pareja(autor,esposa_autor).
pareja(X,Y) :- esposo(X,Y);esposa(Y,X).

suegro(X,Y):-hijo(Z,X),pareja(Z,Y).
suegro(esposa_autor,padre_autor).

yerno(X,Y):-suegro(Y,X),hombre(X).

mujer(nueva_hija).
madre(hijastra_autor,nueva_hija).
padre(padre_autor,nueva_hija).
hermano(autor,nueva_hija).
hermanos(X,Y) :- X\==Y,padre(Z,X),padre(Z,Y);madre(Z,X),madre(Z,Y),X\==Y.
nieto(esposa_autor,nueva_hija).
nieto(X,Y):-padre(Y,C),padre(C,X).
abuelo(autor,nueva_hija).
abuela(esposa_autor,nueva_hija).
hombre(hijo_autor).
padre(autor,hijo_autor).
madre(esposa_autor,hijo_autor).
hijo(hijo_autor,esposa_autor).
abuelo(padre_autor,hijo_autor).
hermanos(hijo_autor,hijastra_autor).

cunado(X,Y):-pareja(Y,C),hermanos(X,C).
% hijo autor cunado del padre de autor
cunado(hijo_autor,padre_autor).
nieto(hijo_autor,hijastra_autor).
tio(hijo_autor,autor).
nieto(X,Y):-abuelo(Y,X).
nieto(X,Y):- abuela(Y,X).
nuera(X,Y):-suegro(Y,X),mujer(X).
nuera(esposa_autor,hijastra_autor).

padre(autor,padre_autor).
padre(padre_autor,autor).
hijo(hijastra_autor,autor).
madre(hijastra_autor,autor).
abuela(esposa_autor,hija_nueva).
tio(hijo_autor,hijastra_autor).
tio(A,B):-padre(C,B),hermanos(C,A).
tia(A,B):-madre(C,B),hermanos(C,A).
abuelo(X,Y):-padre(X,K),padre(K,Y).
abuelo(X,Y):-padre(X,K),madre(K,Y).
% abuelo(X,Y):-abuelo(X,C),hermanos(C,Y).
abuela(X,Y):-madre(X,K),madre(K,Y),mujer(X).
abuela(X,Y):-padre(X,K),padre(K,Y),mujer(X).
% abuela(X,Y):-abuela(X,C),hermanos(C,Y).
bisabuelo(X,Y):-padre(X,Z),abuelo(Z,Y).
bisnieto(X,Y):-bisabuelo(Y,X).




