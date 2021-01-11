% Practica 6



% concatena listas 
conc([],X,[X]).
conc([X|D],B,[X|E]) :- conc(D,B,E).

% traducir 
% entrada: 
% [hola,amigo,como estas]
% saldida:
% [hello,friend,how,are,you]

dic(hello,hola).
dic(a,un).
dic(the,el).
dic(of,de).
dic(that,que).
dic(in,en).
dic(on,sobre).
dic(by,por).
dic(no,no).
dic(but,pero).
dic(those,aquellos).
dic(who,quien).
dic(can,puede).
dic(cannot,no_puede).
dic(is,es).
dic(tree, arbol).
dic(cup, taza).
dic(maestro, teacher).
dic(we_are,somos).
dic(logic,logica).
dic(team,equipo).
dic(bird,pajaro).
dic(student,estudiante).
dic(students,estudiantes).
dic(science,ciencias).
dic(dynamite,dinamita).

% la traduccion de una palabra es simetrica,
%  no  estamos considerado sinonimos 
traducir(X,Y) :- dic(X,Y).
traducir(X,Y):-dic(Y,X).
traducir([X],[Y]) :- dic(X,Y).
traducir([X],[Y]) :- dic(Y,X).
traducir([X|XS],[Y|YS]) :- dic(X,Y), traducir(XS,YS).
traducir([X|XS],[Y|YS]) :- dic(Y,X), traducir(XS,YS).

% Probar con la siguiente frase (español-ingles).
% hola , somos, el equipo dinamita de logica, somos estudiantes de ciencias
% Poner en terminal:   traducir([hola,somos,el,equipo,dinamita,de,logica,somos,estudiantes,de,ciencias],Y).

% Probar ingles- español: traducir([hello,we_are,the,team,dynamite,of,logic,we_are,students,of,science],Y).



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
