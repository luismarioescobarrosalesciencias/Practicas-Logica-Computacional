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


% 2. Mundo de los cubos.
% para hacer pruebas con este predicado: mover(amarillo,verde),luego verificar
% sobre(nada,verde), sobre(nada,rosa).

% Hechos dinamicos que se pueden cambiar a lo largo de la ejecucion del programa.
:- dynamic(sobre/2). % Indica que los hechos sobre() son de tipo dinamico.
sobre(nada,amarillo).
sobre(amarillo,rosa).
sobre(rosa,nada).
sobre(nada,verde).
sobre(verde,nada).
sobre(nada,gris).
sobre(gris,rojo).
sobre(rojo,azul).
sobre(azul,nada).

% hastaArriba. Relación que indica si un cubo X esta sobre todos los demás cubos
%              de su pila.
hastaArriba(X) :- sobre(nada,X).

% bloqueado. Relacién que indica que un cubo X no se puede mover pues hay cubos
%            sobre él.
bloqueado(X) :- not(hastaArriba(X)).

% hastaAbajo. Relación que indica si un cubo X esta hasta abajo de la pila de
%             cubos a la que pertenece.
hastaAbajo(X) :- sobre(X,nada).

% mover. Relación que permite mover un cubo X sobre el cubo Y, actualizando las
%        respectivas referencias.
mover(X,X) :- !,fail.
mover(X,Y) :- sobre(X,Y),!,fail.
mover(X,Y) :- bloqueado(X), bloqueado(Y),!,fail.
mover(X,Y) :- sobre(X,XS), quitar(X,XS), assert(sobre(X,Y)), retract(sobre(nada,Y)).

% quitar. Relación auxiliar que elimina un cubo de su lugar original si este no
%         esta bloqueado eliminando el respectivo hecho sobre.
quitar(X,Y) :- retract(sobre(X,Y)), not(bloqueado(X)), assert(sobre(nada,Y)).

/* Sin duda este fue uno de los ejercicios que más nos ha costado implementar en
la practica, ya que sabíamos de antemano que la relación sobre() estaba conformada
por hechos, además que llegamos muy rapidamente a la codificación de hastaArriba()
hastaAbajo() y bloqueado(), ya que cada una de estas dependia de sobre(). Lo complicado
fue cuando no sabíamos como modificar la base de datos que prolog estaba formando
con sobre(). Investigamos en muchas fuentes hasta que dimos con los predicados dynamic,
assert y retract que nos permiten hacer cambios a nuestra base de datos. Finalmente la
implementación de mover dependio de verificar y eliminar las referencias que sobre()
para cada cubo que moviamos, esto gracias a la funcion auxiliar sobre().
*/

% 3. Automata Finito no Determinista.

% Hechos sobre como esta integrado el automata.
inicial(q0).
final(q0).
delta(q0,a,[q1]).
delta(q0,b,[]).
delta(q1,a,[]).
delta(q1,b,[q2]).
delta(q2,a,[q0]).
delta(q2,a,[q3]).
delta(q2,a,[q4]).
delta(q2,b,[]).
delta(q3,a,[q0]).
delta(q3,b,[]).
delta(q4,a,[q3]).
delta(q4,b,[]).

% aceptar. Relacion que determina si una lista de simbolos es aceptado por el automata.
aceptar(S) :- aceptarAux(S,q0,q0).

% aceptarAux. Relación que determina si una lista de simbolos son aceptados por
%             un estado inicial y un estado final.
aceptarAux([],_,_) :- !,fail.
aceptarAux([E],QI,QF) :- delta(QI,E,[QF]).
aceptarAux([E|ES],QI,QF) :- delta(QI,E,[N]), aceptarAux(ES,N,QF).

%Llegue al resultado ya que pense en una especie de camino que deberia de recorrerse
% para llegar de un punto a otro. SI nos daban una lista, teniamos que descomponerla,
% y verificar que partiendo de el estado inicial y con el primer elemento de la lista
% de simbolos si el siguiente estado correspondia a dicho simbolo, y asi recursivamente
% hasta llegar al estado final. Como aceptar solo recibe unalista, pensamos en aceptarAux
% que recibe como referencia los estados inicial (el estado actual en el que se encuentra)
% y el estado final que es al que se quiere llegar al final de descomponerla.
