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

%se mueve X sobre Y y el sobre() dentro del parenteis es la referencia actualizada.
mover(X,Y,(N,sobre(X,Y))) :- quitar(sobre(X,_),N), not(bloqueado(X)), hastaArriba(Y).

%quitarya esta bien implementado  quiarAux(estado anterior, estado despues de aplicar el predicado)
quitar(sobre(X,Y),sobre(nada,Y)) :- not(bloqueado(X)), sobre(X,Y).

%mover X sobre Y (estado de la pila donde estaba X, estado de la pila X sobre Y)
 %quitar(sobre(X,Z),sobre(nada,Z)).

%quitar(X,Y,N) :- not(bloqueado(X)), sobre(X,Y).
%sobre(X,Y) :- discontiguous sobre(X,Y), mover(X,Y,(sobre(X,Y))).
  %quitarAux(sobre(X,Y),sobre(nada,Y))

%abajo(Y,X) :- sobre(X,Y).


%will_be(put(grey, on, green), ([grey|Stack1], [green|Stack2]), (Stack1, [grey,green|Stack2])).
%mover(put(X, on, Y), ([X|Stack1], [Y|Stack2]), (Stack1, [X,Y|Stack2])).

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
