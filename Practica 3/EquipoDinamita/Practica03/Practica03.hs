		--
--
--
module Practica03 where

import Practica02



{----- Formas Normales -----}

-- 1. fnn. Función que devuelve la Forma Normal Negativa de una
--         proposición.
fnn :: Prop -> Prop
fnn (PVar p) = (PVar p)
fnn (PNeg p)
    |esLiteral p = (PNeg p)
    |otherwise = deMorgan (PNeg (fnn (elimEquiv (elimImpl p))))
fnn (POr p q) = POr (fnn (elimEquiv (elimImpl p))) (fnn (elimEquiv (elimImpl q)))
fnn (PAnd p q) = PAnd (fnn (elimEquiv (elimImpl p))) (fnn (elimEquiv (elimImpl q)))
fnn (PImpl p q) = fnn (elimImpl (PImpl p q))
fnn (PEquiv p q) = fnn (elimEquiv (PEquiv p q))

--Función auxuliar que dada una proposicion nos dice si es una literal (tamaño de variables de la
--proposicion=1)
esLiteral :: Prop -> Bool
esLiteral p
    | length (vars p) == 1 || length (vars p) == 0 = True
    | otherwise = False

-- 2. fnc. Función que devuelve la Forma Normal Conjuntiva de una
--         proposición.
fnc :: Prop -> Prop
fnc (PVar p) = (PVar p)
fnc (PNeg p)
    |esLiteral p = (PNeg p)
    |otherwise = fnc (deMorgan (PNeg ((elimEquiv (elimImpl p)))))
fnc (POr p q) = distr (fnc (elimEquiv (elimImpl p))) (fnc (elimEquiv (elimImpl q)))
fnc (PAnd p q) = PAnd (fnc (fnn (elimEquiv (elimImpl p)))) (fnc (fnn (elimEquiv (elimImpl q))))
fnc (PImpl p q) = fnc (elimImpl (PImpl p q))
fnc (PEquiv p q) = fnc (elimEquiv (PEquiv p q))

--Funcion que regresa la funcion aplicando la ley logica de la distribucion, se utilizó la función
--auxiliar distrAux
distr :: Prop -> Prop -> Prop
distr p q
    | esLiteral p && esLiteral q = POr p q
    | otherwise = distrAux p q

--Aplica las leyes distributivas sobre una proposicion
distrAux :: Prop -> Prop -> Prop
distrAux (PAnd a b) q = PAnd (distr a q) (distr b q)
distrAux p (PAnd c d) = PAnd (distr p c) (distr p d)
distrAux (POr a b) q = POr (POr a b) q
distrAux p (POr c d) = POr p (POr c d)

{----- Algoritmo DPLL -----}

-- Definiciones de algunos conceptos.
type Literal = Prop
type Clausula = [Literal]
type Formula = [Clausula]
type Modelo = [Literal]
type Solucion = (Modelo, Formula)

-- 3. unit. Función que aplica la regla unitaria.
--unit :: Solucion -> Solucion
--unit (m, f) = (m ++ [ms | ms <- literales f], elimLiterales f)
unit :: Solucion -> Solucion
unit (m,f) = ( eliminar (m ++ [ms | ms <- literalF f]), elimLiteral f)

-- elimLiterales. Funcion auxiliar qeu elimina las literales de una formula.
--elimLiterales :: Formula -> Formula
--elimLiterales f = [c | c <- f, not (esLiteralC c)]

--Funcion que recibe una formula y elimina las literales de la misma
elimLiteral :: Formula -> Formula
elimLiteral [] = []
elimLiteral [c]
    | esLiteralC c = []
    | otherwise = [c]
elimLiteral (c:cs)
    | esLiteralC c = cs
    | otherwise = [c] ++ elimLiteral cs
-- literales. Funcion auxiliar que devuelve el conjunto de literales de una
--            fórmula.
literales :: Formula -> [Literal]
literales [] = []
literales [c] = [l | l <- c, esLiteralC c]
literales (c:cs) = [l | l <- c, esLiteralC c] ++ literales cs

--Funcion que dada una formula regresa la lista de sus literales
literalF :: Formula -> [Literal]
literalF [] = []
literalF [c] = [l | l <- c, esLiteralC c]
literalF (c:cs) = [l | l <- c, esLiteralC c]

-- esLiteralC. Funcion auxiiar que determina si una clausula es una literal
esLiteralC :: Clausula -> Bool
esLiteralC c = length c == 1

	
-- 4. elim. Función que aplica la regla de eliminación.
elim :: Solucion -> Solucion
elim (m,f) = (m ,elimAux m f)


-- elimAux. Funcion auxiliar que elimina las literales de la formula que se
--         encuentran en el modelo
elimAux :: Modelo -> [Clausula] -> [Clausula]
elimAux m [] = []
elimAux [] f = f
elimAux m [c]
    | elemEnComun m c = []
    | otherwise = [c]
elimAux m (c:cs)
    | elemEnComun m c = [] ++ (elimAux m cs)
    | otherwise = [c] ++ (elimAux m cs)

longitud::Clausula->Int
longitud c=length c
-- elemEnComun. Funcion auxiliar que determina si dos listas tienen al menos
--              un elemento en comun
elemEnComun :: Eq a => [a] -> [a] -> Bool
elemEnComun l [] = False
elemEnComun l [x]
    | x `elem` l = True
    | otherwise = False
elemEnComun l (x:xs) = (elemEnComun l [x]) || (elemEnComun l xs)

-- 5. red. Función que aplica la regla de reducción.--

red :: Solucion -> Solucion
red ([], f) = ([],f)
red (m,f)
	|(length (heads f)>=2)&& (estacontenido m (heads f))=(m,(redaux m (heads f)))   
	|otherwise=(m,(heads f:redaux m (heads (tail f))))

--Funcion auxiliar que elimina de una clausula el inverso del modelo y regresa una formula
redaux::[Literal]->Clausula->Formula	
redaux [] c=[c]
redaux l []=[]
redaux l c=[dflist (inversor l)(c)]
--(length c>=2)&& (estacontenido(inversor l) (c))=[dflist (inversor l)(c)]
--redaux l (c:cs)

dflist::[Literal]->[Literal]->[Literal]
dflist l m= diferencialistas m (l) --l fungue como modelo

-- S|= Pv-S
----S|=P


-- 6. split. Función que aplica la regla de la partición de una literal.
--            Se debe tomar la primer literal que aparezca en la fórmula.
split :: Solucion -> [Solucion]
split (m, f) = camino1(m,f)++camino2(m,f)

--Funcion auxiliar que regresa le primer elemento de una clausula
primeraliteral::Clausula->[Literal]  ---necesita ser [clausula]
--primeraliteral []=[]
--primeraliteral [c]=c
primeraliteral (c:cs)
	|(esLiteral c)==True = [c]
	|otherwise= [convierte(extraeVar c)]	
--	|otherwise=extraeVar c

--Funcion auxliar que regresa regresa la primera lista de litereales de una formula
primeralit::[Clausula]->[Literal]
primeralit (c:cs)=primeraliteral c


--Funcion que dada una solucion regresa toma el primer camino (cabeza de la primer clausula)
camino1::Solucion->[Solucion]
camino1 (m,f)=[(m++primeralit f ,f)]


--genera un camino en forma de solucion
camino1unico::Solucion->Solucion
camino1unico (m,f)=(m++primeralit f ,f)


--eligue la literal inversa de la primer literal de una formula y genera un camino
camino2::Solucion->[Solucion]
camino2 (m,f)=[(m++[inversorvar(heads (primeralit f))],f)]--mmm


--genera un camino con la literal inversa en forma de solucion
camino2unico::Solucion->Solucion
camino2unico (m,f)=(m++[inversorvar(heads (primeralit f))],f)


--Funcion auxiliar que invierte los elementos de una lista de literales
inversor::[Literal]->[Literal]
inversor  (c:cs) =[inversorvar c]

--inversorvar .-Inverte una Literal
inversorvar::Literal->Literal
inversorvar p=deMorgan(PNeg p)

--heads.-obtiene el elemto de la cabeza de una lista	
heads::[a]->a
heads(x:_)=x


--FUncion que dadas dos soluciones, las pega para que aparezcan en una unica
convertirSolucion::Solucion->Solucion->Solucion
convertirSolucion(m,f) (a,b)=(m,f++[a]++b)

--extraeVar.-Extrae la primer variable de una proposicion
extraeVar::Prop->String
extraeVar p = heads (vars p)
--extraeVar::Prop->Prop
--extraeVar p= PVar heads(var p)

--convierte.- convierte un string a un literal
convierte::String->Literal
convierte p= PVar p	



-- 7. conflict. Función que determina si la Solucion llegó a una contradicción.
conflict :: Solucion -> Bool
confict (m,[])=False  --necesitamos de la forma (m,f), f=[]= [[]]
conflict (m, f)
	 |length f==1=estacontenido(inversor m) (heads f) 
	 |otherwise=estacontenido(inversor m)(heads(tail f))--funcio tail que tiene haskell

	
-- 8. success. Función que determina si la fórmula es satisfacible.
success :: Solucion -> Bool
success (m, f) 
	| f == []=True
	| otherwise = False  --Puede que conflict sea false pero eso no hace que la soluion sea success

--9. appDPLL. Función que aplica las reglas anteriores una vez.
appDPLL :: Solucion -> Solucion
appDPLL (m, f)
	|not(literales f ==[]) = (unit(m,f))-- "--Por unit"
	|not(elim(m,f)==(m,f)) =(elim(m,f))--"--Por elim"  
	|not(red(m,f)==(m,f))=(red(m,f))  
	|otherwise=convertirSolucion(camino1unico(m,f))(camino2unico(m,f))

{-- Puntos Extra --}
{--
--dpll. Función que aplica el algoritmo DPLL a una fórmula.
dpll :: Solucion -> Solucion
dpll (m, f) = error "Sin implementar."
--}

--creo que esta ya la habamos hecho
estacontenido:: Eq a=>[a]->[a]->Bool
estacontenido [] list=True  --La nocion es que el vacio esta contenido en cualquier conjunto, en este caso lista
estacontenido (x:xs) list= x `elem` list && estacontenido xs list

--Nos regresa la diferencia A-B
diferencialistas ::  Eq a => [a] -> [a] -> [a]
diferencialistas lista1 lista2 = [x | x <- lista1, noElemento x lista2]


--noElemento.- nos dice si a no es elemento de la lista
noElemento :: Eq a => a -> [a] -> Bool
noElemento x list=not(elem x list)


listasiguales :: Eq a=>[a]->[a]->Bool
listasiguales lista1 lista2 = (estacontenido lista1 lista2) && (estacontenido lista1 lista2)

sonIgualesProp :: Prop -> Prop -> Bool
sonIgualesProp p q
    | (esNegacion (deMorgan p)) && (esNegacion (deMorgan q)) = (esLiteral p) && (esLiteral q) && (listasiguales (vars p) (vars q))
    | not (esNegacion p) && not (esNegacion q) = (esLiteral p) && (esLiteral q) && (listasiguales (vars p) (vars q))
    | otherwise = False



esNegacion :: Prop -> Bool
esNegacion (PNeg p) = True
esNegacion p = False
