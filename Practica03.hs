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

esLiteral :: Prop -> Bool
esLiteral p
    | length (vars p) == 1 || length (vars p) == 0 = True
    | otherwise = False

-- 2. fnc. Función que devuelve la Forma Normal Conjuntiva de una
--         proposición.
fnc :: Prop -> Prop
fnc (PVar p) = (PVar p)
fnc (POr p q) = POr (fnc (fnn p)) (fnc (fnn q))
fnc (PAnd p q) = PAnd (fnc (fnn p)) (fnc (fnn q))

distr :: Prop -> Prop -> Prop
distr p q
    | esLiteral p && esLiteral q = POr p q
    | (p == PAnd a b) = PAnd (POr a q) (POr a b)
    | otherwise = POr p q

{----- Algoritmo DPLL -----}

-- Definiciones de algunos conceptos.
type Literal = Prop
type Clausula = [Literal]
type Formula = [Clausula]
type Modelo = [Literal]
type Solucion = (Modelo, Formula)

-- 3. unit. Función que aplica la regla unitaria.
unit :: Solucion -> Solucion
unit (m, f) = (m ++ [ms | ms <- literales f], elimLiterales   f)

elimLiterales :: Formula -> Formula
elimLiterales f = [c | c <- f, not (esLiteral1 c)]

literales :: [Clausula] -> [Literal]
literales [] = []
literales [c] = [l | l <- c, esLiteral l]
literales (c:cs) = [l | l <- c, esLiteral l] ++ literales cs

clausulas :: Formula -> [Clausula]
clausulas f = [c | c <- f, esLiteral1 c]

esLiteral1 :: Clausula -> Bool
esLiteral1 [l] = length [l] == 1 && esLiteral l

-- 4. elim. Función que aplica la regla de eliminación.
elim :: Solucion -> Solucion
elim (m, f) = error "Sin implementar."

-- 5. red. Función que aplica la regla de reducción.
red :: Solucion -> Solucion
red (m, f) = error "Sin implementar."

-- 6. split. Función que aplica la regla de la partición de una literal.
--            Se debe tomar la primer literal que aparezca en la fórmula.
split :: Solucion -> [Solucion]
split (m, f) = error "Sin implementar."

-- 7. conflict. Función que determina si la Solucion llegó a una contradicción.
conflict :: Solucion -> Bool
conflict (m, f) = error "Sin implementar."

-- 8. success. Función que determina si la fórmula es satisfacible.
success :: Solucion -> Bool
success (m, f) = error "Sin implementar."

--9. appDPLL. Función que aplica las reglas anteriores una vez.
appDPLL :: Solucion -> Solucion
appDPLL (m, f) = error "Sin implementar."



{-- Puntos Extra --}
{--
--dpll. Función que aplica el algoritmo DPLL a una fórmula.
dpll :: Solucion -> Solucion
dpll (m, f) = error "Sin implementar."
--}
