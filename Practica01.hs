-- Equipo Dinamita
-- Integrantes:
--  Escobar Rosales Luis Maro
--  Liera Montaño Miguel Ángel
-- Práctica01

module Practica01 where

--primitivo. Función que recibe un entero y devuelve su primitivo.
primitivo :: Int -> Int
primitivo n
  | n > 10 = primitivo (multiplica (multiplica n))
  | otherwise = n


--multiplica Funcion que multiplica los digitos de un entero,
multiplica ::Int -> Int
multiplica 0 = 1 --esto es necesario para que las multiplicaciones de los siguientes casos no se multipliquen por 0
multiplica n = (mod n 10) * multiplica(div n 10)  --Modulo n y 10 multiplicado por la division de n entre 10, lo cual nos terminara dando un numero con 2 digitos


--area. Función que recibe tres puntos y devuelve el área del
--      triángulo formado.
area :: (Double, Double) -> (Double, Double) -> (Double, Double) -> Double
area (x1, y1) (x2, y2) (x3, y3) = (((x1 * y2) + (x2 * y3) + (x3 * y1)) - ((x1 * y3) + (x3 * y2) +(x2 * y1))) / 2
-- para esta operacion se utilizo la formula vectorial para obtener el area del triangulo


--Funcion aulixiar que elimina caracteres repetidos de una lista
eliminar::Eq a =>[a]->[a] --La firma no esta hecha String ->String  por si se llegara a necesitar esta funcion con alguna otro tipo de listas
eliminar a =remover a [] --mandamos dos listas, la lista que recibimos inicialmente y una lista vacia

--Funcion remover, funcion auxiliar para remover elementos repetidos
-- x`elem`y evalua si x es elemento de y
remover::Eq a=>[a]->[a]->[a]
remover [] a=[]
remover (x:xs) listaaux
	|(x`elem`listaaux) = remover xs listaaux --Si x es elemento de la lista auxiliar entonces llamamos a remover con los argumentos xs (cola de la lista [a]) y listaauxiliar
	|otherwise = x:(remover xs (x:listaaux)) --Si x no es elemento de la lista auxiliar entonces concatenamos x con remover de xs con (x:listaaux)
							--en este caso actualizamos la lista auxiliar guardando la cabeza de nuestra primera lista (de no hacerlo asi no se eliminaria nada)

--heterograma. Función que recibe una cadena y lo convierte en un
--             heterograma.
heterograma :: String -> String
heterograma a = eliminar a  --Se utilizaron las funciones auxiliares eliminar y remover



--bolsa. Función que recibe una cadena y devuelve una lista de tuplas
--       con el número de ocurrencias de cada letra de la palabra.
bolsa :: String -> [(Char, Int)]
bolsa [] = []
bolsa (x:xs) =  [(x,apariciones x (x:xs))] ++ bolsa (filter (/= x) xs)
--filter es utilizado para que la letra no vuelva a contemplarse en lo que resta de la lista
-- y que no vuelva a contar las repeticiones de la letra en la cadena

--apariciones. Función que recibe una letra y ua cadena y devuelve el nuemero de
--             apariciones de la letra en la lista
apariciones :: Char -> String -> Int
apariciones c [] = 0
apariciones c (x:xs)
    | c == x = 1 + apariciones c xs
    | otherwise = apariciones c xs

--Es la misma funcion reversa que vimos  en la clase pero esta recibe una lista y devuelve una lista
reversa :: [a]-> [a]
reversa [] = []
reversa (x:xs) = reversa xs ++ [x]
--esPalindromo. Función que verifica si una cadena es palíndromo.
esPalindromo :: Eq a => [a] -> Bool
esPalindromo [] =True --El caso base es True, ya que de lo contrario
esPalindromo a = a==reversa(a)  --Si  a que esta actuando como lista [] es igual a la reversa de a que igual actua como lista  entonces es palindromo


--diferencia. Función que devuelve una lista con la diferencia entre
--            dos listas.
diferencia :: Eq a => [a] -> [a] -> [a]
diferencia [] [] = []
diferencia [a] [] = []
diferencia [] [b] = [b]
diferencia [a] [b]
-- este caso contempla cuando dos listas tienen un solo elemento
    | not (contiene b [a]) = [b]
    | otherwise = []
-- estos dos siguientes casos contemplan cuando una de las dos listas contiene un solo elemento
diferencia [a] (y:b)
    | not (contiene y [a]) = [y] ++ diferencia [a] b
    | otherwise = diferencia [a] b
diferencia (x:a) [b]
    | not (contiene b (x:a)) = [b]
    | otherwise = []
diferencia (x:a) (y:b)
    | not (contiene y (x:a)) = [y] ++ diferencia (x:a) b
    | otherwise = diferencia (x:a) b

--contiene. Función auxiliar que verifica si una lista contiene o no a un
--          elemento
contiene :: Eq a => a -> [a] -> Bool
contiene x [] = False
contiene x (y:b)
    | x == y = True
    | otherwise = contiene x b


--primos. Función que devuelve una lista con todos los números primos
--        hasta n.
primos :: Int -> [Int]
primos n = [x | x <- [2..n], esPrimo x] --Se utilizó la funcion auxiliar esPrimo

-- factores. Función  auxiliar que idica todos los factores por los cuales es
--           divisible numero.
factores :: Int -> [Int]
factores n = [x | x <- [1..n], n `mod` x == 0]

-- esPrimo. Función  auxiliar que indica si un numero es o no un primo a partir
--          de verificar si es divisible entre solamente 1 y si mismo.
esPrimo :: Int -> Bool
esPrimo n = factores n == [1,n]


{-- Definición de Binario.--}
data Binario = U | Cero Binario | Uno Binario


--Instancia de la clase Show para Binario.
instance Show Binario where
	show U = "1"
 	show (Cero b) = show b ++ "0"
  	show (Uno b) = show b ++ "1"
--Funcion axuliar, la vimos en clase, regresa el sucesor
sucesor :: Binario -> Binario
sucesor U = (Cero U) -- Cero U == 01
sucesor (Cero b) = (Uno b) --Cero U es 10, por la def de show, luego nos regresa Uno b que es 11
sucesor (Uno b) = (Cero (sucesor b)) --recursivo

--suma. Función que devuelve la suma de dos Binarios.
suma :: Binario -> Binario -> Binario
suma b U = sucesor(b) --caso base , esto es igual a sucesor U = Cero U, esto nos sirve para "sumar un mas 1" ya que en nuestra definicion U=1,
suma U b= sucesor (b)	--caso base, esto es iguala sucesor U =Cero U
suma (Cero a) (Cero b) = Cero( suma a b) --llamada recursiva
suma (Cero a) (Uno b) = Uno(suma a b) --llamada recursiva
suma(Uno a) (Cero b) = Uno(suma a b)
suma (Uno a) (Uno b)= Cero(sucesor(suma a b))



{-- Definición del Árbol binario.--}
data Arbol a = Vacio | Nodo a (Arbol a) (Arbol a) deriving(Show)


--inOrden. Función que convierte un árbol binario en una lista por
--         su recorrido in-orden.
inOrden :: Arbol a -> [a]
inOrden Vacio = []
inOrden (Nodo r Vacio Vacio) = [r]
inOrden (Nodo r i d) = inOrden i ++ [r] ++ inOrden d
