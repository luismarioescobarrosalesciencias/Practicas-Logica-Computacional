-- *Nombre del equipo/alumno*
-- *Práctica*

module Practica01 where

--primitivo. Función que recibe un entero y devuelve su primitivo.
primitivo :: Int -> Int
primitivo n = error "Sin implementar."


--area. Función que recibe tres puntos y devuelve el área del
--      triángulo formado.
area :: (Double, Double) -> (Double, Double) -> (Double, Double) -> Double
area (x1, y1) (x2, y2) (x3, y3) = sqrt((x1-x2)^2+(y1-y2)^2) * sqrt((x2-x3)^2+(y2-y3)^2) / 2


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
diferencia [a] [] = [a]
diferencia [] [b] = [b]
diferencia (x:a) (y:b)
    | x /= y = (y:diferencia a b)
    |otherwise = diferencia a b


--primos. Función que devuelve una lista con todos los números primos
--        hasta n.
primos :: Int -> [Int]
primos n = error "Sin implementar."



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
inOrden t = error "Sin implementar."
