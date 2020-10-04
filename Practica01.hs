-- *Nombre del equipo/alumno*
-- *Práctica*

module Practica01 where

--primitivo. Función que recibe un entero y devuelve su primitivo.
primitivo :: Int -> Int
primitivo n
   |n < 10 = n
   |otherwise = primitivo (n `mod` 10 * primitivo (n `div` 10))

--area. Función que recibe tres puntos y devuelve el área del
--      triángulo formado.
area :: (Double, Double) -> (Double, Double) -> (Double, Double) -> Double
area (x1, y1) (x2, y2) (x3, y3) = (((x1 * y2) + (x2 * y3) + (x3 * y1)) - ((x1 * y3) + (x3 * y2) +(x2 * y1))) / 2

--heterograma. Función que recibe una cadena y lo convierte en un
--             heterograma.
heterograma :: String -> String
heterograma s = error "Sin implementar."


--bolsa. Función que recibe una cadena y devuelve una lista de tuplas
--       con el número de ocurrencias de cada letra de la palabra.
--bolsa :: Eq a => String -> [(Char, Int)]
--bolsa s = error "Sin implementar."


--esPalindromo. Función que verifica si una cadena es palíndromo.
esPalindromo :: Eq a => [a] -> Bool
esPalindromo s = error "Sin implementar."


--diferencia. Función que devuelve una lista con la diferencia entre
--            dos listas.
diferencia :: Eq a => [a] -> [a] -> [a]
diferencia [] [] = []
diferencia [a] [] = []
diferencia [] [b] = [b]
diferencia [a] [b]
    | not (contiene b [a]) = [b]
    | otherwise = []
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
primos n = [x | x <- [2..n], esPrimo x]

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
	show b = error "Sin implementar."


--suma. Función que devuelve la suma de dos Binarios.
suma :: Binario -> Binario -> Binario
suma b1 b2 = error "Sin implementar."



{-- Definición del Árbol binario.--}
data Arbol a = Vacio | Nodo a (Arbol a) (Arbol a) deriving(Show)


--inOrden. Función que convierte un árbol binario en una lista por
--         su recorrido in-orden.
inOrden :: Arbol a -> [a]
inOrden Vacio = []
inOrden (Nodo r Vacio Vacio) = [r]
inOrden (Nodo r i d) = inOrden i ++ [r] ++ inOrden d
