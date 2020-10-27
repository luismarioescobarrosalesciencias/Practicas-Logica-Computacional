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
    | contiene x (vars (PVar p)) = True
    | otherwise = interp xs (PVar p)
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
modelos p = [i | i <- estados p, interp i p == True]

--6. tautologia. Función que dice si una proposición es tautología.
tautologia :: Prop -> Bool
tautologia p = estados p == modelos p

--7. satisfen. Función que resuelve si una proposición es satisfacible
-- 				con cierto estado.
satisfen :: Estado -> Prop -> Bool
satisfen e p = interp e p == True

--8. satisf. Función que resuelve si una proposición es satisfacible.
satisf :: Prop -> Bool
satisf p = modelos p /= []

--9. insatisfen. Función que resuelve si una proposición es insatisfacible
-- 					con cierto estado.
insatisfen :: Estado -> Prop -> Bool
insatisfen e p = interp e p == False

--10. contrad. Función que dice si una proposición es una contradicción.
contrad :: Prop -> Bool
contrad p = modelos p == []

--11. equiv. Función que devuelve True si dos proposiciones son equivalentes.
equiv :: Prop -> Prop -> Bool
equiv p1 p2 = (modelos p1 == modelos p2) || contenido (modelos p1) (modelos p2) || contenido (modelos p2) (modelos p1)

-- contenido. Funcion auxiliar que permite saber si un conjunto esta contenido en otro
contenido :: Eq a => [a] -> [a] -> Bool
contenido [x] [] = False
contenido [] [x] = False
contenido [] [] = True
contenido [x] (y:ys) = contiene x (y:ys)
contenido (x:xs) [y] = contiene y (x:xs)
contenido (x:xs) (y:ys) = contiene x (y:ys) && contenido xs (y:ys)

--12. elimEquiv. Función que elimina las equivalencias lógicas.
elimEquiv :: Prop -> Prop
elimEquiv PTrue =PTrue
elimEquiv PFalse=PFalse
elimEquiv (PVar p)= PVar p
elimEquiv (PNeg p)=PNeg(elimEquiv p)
elimEquiv (POr p q)=POr(elimEquiv p) (elimEquiv q)
elimEquiv (PAnd p q)=PAnd(elimEquiv p) (elimEquiv q)
elimEquiv(PImpl p q)=PImpl(elimEquiv p) (elimEquiv q)
elimEquiv(PEquiv p q)= PAnd(elimEquiv(PImpl p q)) (elimEquiv(PImpl q p))

--13. elimImpl. Función que elimina las implicaciones lógicas.
elimImpl :: Prop -> Prop
elimImpl PTrue= PTrue
elimImpl PFalse=PFalse
elimImpl (PVar p) = PVar p
elimImpl (PNeg p)= PNeg(elimImpl p)
elimImpl(POr p q)= POr(elimImpl p) (elimImpl q)
elimImpl (PAnd p q) = PAnd(elimImpl p)(elimImpl q)
elimImpl(PImpl p q)= POr(elimImpl (PNeg p))(elimImpl q)
elimImpl(PEquiv p q)= PEquiv(elimImpl p)(elimImpl q)

--14. deMorgan. Función que aplica las leyes de DeMorgan a una proposición.
deMorgan :: Prop -> Prop
deMorgan (PVar p)=PVar p
deMorgan (PNeg (PVar p))= PNeg(PVar p)
deMorgan(PNeg (PNeg p))= p
deMorgan(PNeg (POr p q))= PAnd (deMorgan( PNeg p))(deMorgan(PNeg q))
deMorgan(PNeg (PAnd p q))= POr (deMorgan (PNeg p))(deMorgan(PNeg q))
deMorgan p = p

{-- Punto extra--}

estadosConj :: [Prop] -> [Estado]
estadosConj [] = []
estadosConj [p] = estados p
estadosConj (p:ps) = (estados p) ++ (estadosConj ps)


unirlistas:: Eq a=>[a]->[a]->[a]
unirlistas b []=b
unirlistas [] b= b
unirlistas [a] [b] =  [a] ++ [b]
unirlistas (x:xs) (y:ys) =  (x:xs) ++ (y:ys)
uniirlistas (x:xs) b
	|elem x b = unirlistas xs b
	|otherwise= x:unirlistas xs b

modelosConj :: [Prop] -> [Estado]
modelosConj [p] = [i | i <- estadosConj [p], interpConj [i] [p] == True]
{-- modelosConj [] =[]
modelosConj [p]= modelos p
modelosConj (p:ps)
   |(contenido [(vars p)] (varsConj ps)) && (contenido (modelos p) (modelosConj ps)) = (modelos p) ++ (modelosConj ps)
   |(contenido [(vars p)] (varsConj ps)) && not(contenido (modelos p) (modelosConj ps)) = []
   |otherwise = []--}

interpConj :: [Estado] -> [Prop] -> Bool
interpConj [] [p] = interp [] p
interpConj [e] [p] = interp e p
interpConj [e] (p:ps) = (interp e p) && (interpConj [e] ps)
interpConj (x:xs) (y:ys) = (interp x y) && (interpConj [x] ys) && (interpConj xs [y]) && (interpConj xs ys)

varsConj :: [Prop] -> [Estado]
varsConj [p] = eliminar [vars p]
varsConj (p:ps) = eliminar ([(vars p)] ++ (varsConj ps))

satisfenConj:: Estado -> [Prop] -> Bool
--satisfenConj e [] =
satisfenConj e [p] = (satisfen e p)
satisfenConj e (p:ps)= (satisfen e p) && (satisfenConj e ps)

satisfConj:: [Prop] -> Bool
satisfConj [] = False
satisfConj [p] = (satisf p)
-- satisfConj (p:ps) = contenido (modelos p) (satisfConj ps)


insatisfenConj:: Estado -> [Prop] -> Bool
insatisfenConj e (p:ps)
	|(satisfenConj e (p:ps)==True )=False
	|otherwise =True

insatisfConj:: [Prop] -> Bool
insatisfConj (p:ps)
	|(satisfConj (p:ps) ==True) =False
	| otherwise = True

--consecuencia. Función que determina si una proposición es consecuencia
--				del conjunto de premisas.
consecuencia:: [Prop] -> Prop -> Bool
consecuencia gamma phi = null [i | i <- estadosConj (phi : gamma),
								satisfenConj i gamma,
								not (satisfen i phi)]

--argCorrecto. Función que determina si un argumento es lógicamente
--				correcto dadas las premisas.
argCorrecto :: [Prop] -> Prop -> Bool
argCorrecto gamma psi = consecuencia gamma psi
