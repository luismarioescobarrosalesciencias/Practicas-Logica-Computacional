% Practica 5

traducir(X,Y) :- dic(X,Y).
traducir([X],[Y]) :- dic(X,Y).
traducir([X|XS],[Y|YS]) :- dic(X,Y), traducir(XS,YS).

% conc. Concatena dos listas.
conc([],X,[X]).
conc([X|D],B,[X|E]) :- conc(D,B,E).

dic(tree, arbol).
dic(cup, taza).
dic(maestro, teacher).

matematico(pepe).
sabemate(X) :- matematico(X).

sobre(nada,amarillo).
sobre(amarillo,rosa).
sobre(rosa,nada).
sobre(nada,verde).
sobre(verde,nada).
sobre(nada,gris).
sobre(gris,rojo).
sobre(rojo,azul).
sobre(azul,nada).

hastaArriba(X) :- sobre(nada,X).

bloqueado(X) :- not(hastaArriba(X)).

hastaAbajo(X) :- sobre(X,nada).

mover(X,Y) :- not(bloqueado(X)), hastaArriba(Y), quitar(X,Y), sobre(X,Y).

quitar(X,Y) :- not(bloqueado(X)), sobre(X,Y).
abajo(Y,X) :- sobre(X,Y).
