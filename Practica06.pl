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

%sobre(X,Y,[X,N]) :- sobre(nada,Y,N).

%sobre(X,[nada],[X,nada]).
%sobre(X,[Y|D],[X|E]) :- sobre(Y,D,E).

%sobre(X,Y,[X|Y]).
%sobre(X,)
%sobre(X,Y,N) :- sobre(X,)
%mover((X,Y), (sobre(X,XS,[X|XS]), sobre(Y,YS,[Y|YS])), (sobre(XS,XS), sobre(X,Y))).
%will_be(Command, Before_state, After_state)


/*lista([]):-!.
lista([_|Y]):-lista(Y).

elimina(X,[X|T],T).
my_remove_element(_, [], []).

my_remove_element(Y, [Y|Xs], Zs):-
          my_remove_element(Y, Xs, Zs), !.

my_remove_element(X, [Y|Xs], [Y|Zs]):-
          my_remove_element(X, Xs, Zs).*/
