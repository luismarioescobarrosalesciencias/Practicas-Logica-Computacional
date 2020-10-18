--
--
module Practica02 where

--Prop. Tipo de datos para proposiciones lógicas.
data Prop = PTrue | PFalse | PVar String | PNeg Prop | POr Prop Prop
                  | PAnd Prop Prop | PImpl Prop Prop | PEquiv Prop Prop

--Estado. Lista de variables asignadas como verdaderas.
type Estado = [String]

--Instancia Show para Prop.
instance Show Prop where
  show PTrue = "true"
  show PFalse = "false "
  show (PVar x) = show x
  show (PNeg p) = "¬"++ (show p)
  show (POr p1 p2) = "(" ++ show p1 ++ " v " ++ show p2 ++ ")"
  show (PAnd p1 p2) = "(" ++ show p1 ++ " ^ " ++ show p2 ++ ")"
  show (PImpl p1 p2) = "(" ++ show p1 ++ " -> " ++ show p2 ++ ")"
  show (PEquiv p1 p2) = "(" ++ show p1 ++ " <--> " ++ show p2 ++ ")"


--1. interp. Función que evalua una proposición dado el estado.
interp :: Estado -> Prop -> Bool
interp [] (PVar p) = False
interp (x:xs) PTrue = True
interp (x:xs) PFalse = False
interp (x:xs) (PVar p)
    | contiene x [x] && contiene x (vars (PVar p)) = True
    | otherwise = False
interp (x:xs) (PNeg p) = not (interp (x:xs) p)
interp [] (PNeg p) = not (interp [] p)
interp (x:xs) (POr p q) = (interp (x:xs) p) || (interp (x:xs) q)
interp [] (POr p q) = (interp [] p) || (interp [] q)
interp (x:xs) (PAnd p q) = (interp (x:xs) p) && (interp (x:xs) q)
interp [] (PAnd p q) = (interp [] p) && (interp [] q)
interp (x:xs) (PImpl p q) = (not (interp (x:xs) p)) || (interp (x:xs) q)
interp [] (PImpl p q) = (not (interp [] p)) || (interp [] q)
interp (x:xs) (PEquiv p q) = ((interp (x:xs) p) && (interp (x:xs) q)) || ((not (interp (x:xs) p)) && (not (interp (x:xs) q)))
interp [] (PEquiv p q) = ((interp [] p) && (interp [] q)) || ((not (interp [] p)) && (not (interp [] q)))

--contiene. Función auxiliar que verifica si una lista contiene o no a un
--          elemento
contiene :: Eq a => a -> [a] -> Bool
contiene x [] = False
contiene x (y:b)
    | x == y = True
    | otherwise = contiene x b

--2. estados. Función que devuelve una lista de todas las combinaciones
-- 				posibles de los estados de una proposición.
estados :: Prop -> [Estado]
estados PTrue  = [vars PTrue]
estados PFalse  = [vars PFalse]
estados (PVar p)  = [vars (PVar p)]
estados (PNeg p) = subconj (vars (PNeg p))
estados (POr p q) = subconj (vars (POr p q))
estados (PAnd p q) = subconj (vars (PAnd p q))
estados (PImpl p q) = subconj (vars (PImpl p q))
estados (PEquiv p q) = subconj (vars (PEquiv p q))

--3. vars. Función que obtiene la lista de todas las variables de una
--			proposición.
vars :: Prop -> [String]
vars PTrue = []
vars PFalse = []
vars (PVar p) = [p]
vars (PNeg p) = eliminar (vars p)
vars (POr p q) = eliminar (vars p ++ vars q)
vars (PAnd p q) = eliminar (vars p ++ vars q)
vars (PImpl p q) = eliminar (vars p ++ vars q)
vars (PEquiv p q) = eliminar (vars p ++ vars q)

--Funcion aulixiar que elimina caracteres repetidos de una lista
eliminar::Eq a =>[a]->[a] --La firma no esta hecha String ->String  por si se llegara a necesitar esta funcion con alguna otro tipo de listas
eliminar a =remover a [] --mandamos dos listas, la lista que recibimos inicialmente y una lista vacia

--Funcion auxiliar remover, funcion auxiliar para remover elementos repetidos
-- x`elem`y evalua si x es elemento de y
remover::Eq a => [a] -> [a] -> [a]
remover [] a=[]
remover (x:xs) listaaux
    |(x`elem`listaaux) = remover xs listaaux --Si x es elemento de la lista auxiliar entonces llamamos a remover con los argumentos xs (cola de la lista [a]) y listaauxiliar
    |otherwise = x:(remover xs (x:listaaux)) --Si x no es elemento de la lista auxiliar entonces concatenamos x con remover de xs con (x:listaaux)
    --en este caso actualizamos la lista auxiliar guardando la cabeza de nuestra primera lista (de no hacerlo asi no se eliminaria nada)


--4. subconj. Función que devuelve el conjunto potencia de una lista.
subconj :: [a] -> [[a]]
subconj [] = [[]]
subconj (x:xs) = [x:as | as <- subconj xs] ++ subconj xs


--5. modelos. Función que devuelve la lista de todos los modelos posibles
-- 				para una proposición.
modelos :: Prop -> [Estado]
modelos p = error "Sin implementar."

--6. tautologia. Función que dice si una proposición es tautología.
tautologia :: Prop -> Bool
tautologia p = error "Sin implementar."

--7. satisfen. Función que resuelve si una proposición es satisfacible
-- 				con cierto estado.
satisfen :: Estado -> Prop -> Bool
satisfen e p = error "Sin implementar."

--8. satisf. Función que resuelve si una proposición es satisfacible.
satisf :: Prop -> Bool
satisf p = error "Sin implementar."

--9. insatisfen. Función que resuelve si una proposición es insatisfacible
-- 					con cierto estado.
insatisfen :: Estado -> Prop -> Bool
insatisfen e p = error "Sin implementar."

--10. contrad. Función que dice si una proposición es una contradicción.
contrad :: Prop -> Bool
contrad p = error "Sin implementar."

--11. equiv. Función que devuelve True si dos proposiciones son equivalentes.
equiv :: Prop -> Prop -> Bool
equiv p1 p2 = error "Sin implementar."

--12. elimEquiv. Función que elimina las equivalencias lógicas.
elimEquiv :: Prop -> Prop
elimEquiv p = error "Sin implementar."

--13. elimImpl. Función que elimina las implicaciones lógicas.
elimImpl :: Prop -> Prop
elimImpl p = error "Sin implementar."

--14. deMorgan. Función que aplica las leyes de DeMorgan a una proposición.
deMorgan :: Prop -> Prop
deMorgan p = error "Sin implementar."


{-- Punto extra--}
{--
estadosConj :: [Prop] -> [Estado]
modelosConj :: [Prop] -> [Estado]
satisfenConj:: Estado -> [Prop] -> Bool
satisfConj:: [Prop] -> Bool
insatisfenConj:: Estado -> [Prop] -> Bool
insatisfConj:: [Prop] -> Bool

--consecuencia. Función que determina si una proposición es consecuencia
--				del conjunto de premisas.
consecuencia: [Prop] -> Prop -> Bool
consecuencia gamma phi = null [i | i <- estadosConj (phi : gamma),
								satisfenConj i gamma,
								not (satisfen i phi)]

--argCorrecto. Función que determina si un argumento es lógicamente
--				correcto dadas las premisas.
argCorrecto :: [Prop] -> Prop -> Bool
argCorrecto gamma psi = consecuencia gamma psi
--}
