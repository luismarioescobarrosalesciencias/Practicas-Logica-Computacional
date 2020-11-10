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

distr :: Prop -> Prop -> Prop
distr p q
    | esLiteral p && esLiteral q = POr p q
    | otherwise = distrAux p q

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
--unit (m, f) = (eliminar (m ++ [ms | ms <- literales f]), elimLiterales f)

unit :: Solucion -> Solucion
unit (m,f) = ( eliminar (m ++ [ms | ms <- literalF f]), elimLiteral f)

-- elimLiterales. Funcion auxiliar qeu elimina las literales de una formula.
--elimLiterales :: Formula -> Formula
--elimLiterales f = [c | c <- f, not (esLiteralC c)]

-- elimLiteral. Funcion auxiliar que elimina la primera literal de una formula
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
--literales :: Formula -> [Literal]
--literales [] = []
--literales [c] = [l | l <- c, esLiteralC c]
--literales (c:cs) = [l | l <- c, esLiteralC c] ++ literales cs

-- literalF. Funcion auxiliar que devuelve la primera literal de una formula
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

-- elemEnComun. Funcion auxiliar que determina si dos listas tienen al menos
--              un elemento en comun
elemEnComun :: Eq a => [a] -> [a] -> Bool
elemEnComun l [] = False
elemEnComun l [x]
    | x `elem` l = True
    | otherwise = False
elemEnComun l (x:xs) = (elemEnComun l [x]) || (elemEnComun l xs)

-- 5. red. Función que aplica la regla de reducción.
red :: Solucion -> Solucion
red (m, f) = error "Sin implementar."

--redAux :: Eq Literal => Modelo -> Clausula -> Clausula
--redAux m c = [l | l <- c, (l `elem` m)]

--complemento :: Eq Literal => Literal -> Modelo -> Bool
--complemento

-- 6. split. Función que aplica la regla de la partición de una literal.
--            Se debe tomar la primer literal que aparezca en la fórmula.
split :: Solucion -> [Solucion]
split (m, f) = error "Sin implementar."

-- 7. conflict. Función que determina si la Solucion llegó a una contradicción.
conflict :: Solucion -> Bool
--conflict (m, f) = not (success (m,f))
conflict (m, f) = error "Sin implementar."
-- 8. success. Función que determina si la fórmula es satisfacible.
success :: Solucion -> Bool
success (m, f) = error "Sin implementar."
--    | f == [] = True
--    | otherwise = false

--9. appDPLL. Función que aplica las reglas anteriores una vez.
appDPLL :: Solucion -> Solucion
appDPLL (m, f) = error "Sin implementar."


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

diferencialistas ::  Eq a => [a] -> [a] -> [a]
diferencialistas lista1 lista2 = [x | x <- lista1, noElemento x lista2]

noElemento :: Eq a => a -> [a] -> Bool
noElemento x list=not(elem x list)

--listasiguales::tacontenido:: Eq a=>[a]->[a]->Bool
--listasiguales lista1 []=False
--listasiguales lista1 lista2
--	|diferencialistas(lista1 lista2)=[]=true
--	|otherwise False

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
