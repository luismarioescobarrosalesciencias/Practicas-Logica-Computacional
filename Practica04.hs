--
--
module Practica04 where

--Definición del tipo de datos para términos.
data Term = V Nombre | F Nombre [Term]

--Definición del tipo de datos para fórmulas.
data Form = NForm | TrueF | FalseF | Pr Nombre [Term] | Eq Term Term |
            Neg Form | Conj Form Form | Disy Form Form |
            Imp Form Form | Equi Form Form | All Nombre Form |
            Ex Nombre Form

type Nombre = String

type Subst = [(Nombre,Term)]


--Instancia Show para Term.
instance Show Term where
  show (V x) = x
  show (F f t) = f ++ "(" ++ show t ++ ")"

--Instancia Show para Form.
instance Show Form where
  show NForm = ""
  show TrueF = "T"
  show FalseF = "F"
  show (Pr p t) = p ++ "(" ++ show t ++ ")"
  show (Eq t1 t2) = "(" ++ show t1 ++ "=" ++ show t2 ++ ")"
  show (Neg f) = "¬" ++ show f
  show (Conj f1 f2) = "(" ++ show f1 ++ " ^ " ++ show f2 ++ ")"
  show (Disy f1 f2) = "(" ++ show f1 ++ " v " ++ show f2 ++ ")"
  show (Imp f1 f2) = "(" ++ show f1 ++ " -> " ++ show f2 ++ ")"
  show (Equi f1 f2) = "(" ++ show f1 ++ " <--> " ++ show f2 ++ ")"
  show (All x f) = "Alle " ++ x ++ " (" ++ show f ++ ")"
  show (Ex x f) = "Ein " ++ x ++ " (" ++ show f ++ ")"



--alcance. Función que devuelve el alcance de los cuantificadores de
--          una fórmula.
alcance :: Form -> [(Form, Form)]
alcance NForm = []
alcance TrueF = []
alcance FalseF = []
alcance (Pr p t) = []
alcance (Eq t1 t2) = []
alcance (Neg f) = []
alcance (Conj f1 f2) = []
alcance (Disy f1 f2) = []
alcance (Imp f1 f2) = []
alcance (Equi f1 f2) = []
alcance (Ex p f) = [((Ex p NForm),f)] ++ alcance f
alcance (All p f) = [((All p NForm),f)] ++ alcance f

--bv. Función que devuelve las variables ligadas de una fórmula.
bv :: Form -> [Nombre]
bv NForm = []
bv TrueF = []
bv FalseF = []
bv (Pr p t) = []
bv (Eq t1 t2) = []
bv (Neg f) = []
bv (Conj f1 f2) = []
bv (Disy f1 f2) = []
bv (Imp f1 f2) = []
bv (Equi f1 f2) = []
bv (Ex p f) = eliminar ([p] ++ bv f)
bv (All p f) = eliminar ([p] ++ bv f)

--fv. Función que devuelve las variables libres de una fórmula.
fv :: Form -> [Nombre]
fv f = eliminar (diferencia (varsForm f) (bv f))

diferencia :: Eq a => [a] -> [a] -> [a]
diferencia xs ys = zs
    where zs = [x | x <- xs, x `notElem` ys]

-- varsForm. Funcion auxiliar que calculalas variables de una formula
varsForm :: Form -> [Nombre]
varsForm NForm = []
varsForm TrueF = []
varsForm FalseF = []
varsForm (Pr p t) = varsTermConj t
varsForm (Eq t1 t2) = varsTerm t1 ++ varsTerm t2
varsForm (Neg f) = varsForm f
varsForm (Conj f1 f2) = varsForm f1 ++ varsForm f2
varsForm (Disy f1 f2) = varsForm f1 ++ varsForm f2
varsForm (Imp f1 f2) = varsForm f1 ++ varsForm f2
varsForm (Equi f1 f2) = varsForm f1 ++ varsForm f2
varsForm (Ex p f) = [p] ++ varsForm f
varsForm (All p f) = [p] ++ varsForm f

-- varsTerm. Funcion auxiliar que calcula las variables de un termino
varsTerm :: Term -> [Nombre]
varsTerm (V x) = [x]
varsTerm (F f [t]) = varsTerm t
varsTerm (F f (t:ts)) = varsTerm t ++ varsTermConj ts

-- varsTermConj. Funcion auxiliar que calcula variables de un conjunto de terminos
varsTermConj :: [Term] -> [Nombre]
varsTermConj [t] = varsTerm t
varsTermConj (x:xs) = varsTerm x ++ varsTermConj xs

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

--sustTerm. Función que realiza la sustitución de variables en un término.
sustTerm :: Term -> Subst -> Term
--sustTerm [] s = s
sustTerm t [] = t
sustTerm (V x) [(v,ts)]
    | listasiguales x v = ts
    | otherwise = (V x)
sustTerm (F f t) s = (F f (sustTermAux t s))

--{
--sustTermAux :: [Term] -> Subst -> [Term]
-- sustTermAux [t] [(v,ts)] = [sustTerm t [(v,ts)]]
--sustTermAux (t:tc) [(v,ts)] = [sustTerm t [(v,ts)]] ++ sustTermAux tc [(v,ts)]
-- sustTermAux (x:xs) (y:ys)
    -- | sustTermAux [x] [y] == [x] = sustTermAux [x] ys ++ sustTermAux xs [y] ++ sustTermAux xs ys
    -- | otherwise = sustTermAux [x] [y] ++ sustTermAux [x] ys ++ sustTermAux xs [y] ++ sustTermAux xs ys


--contiene. Función auxiliar que verifica si una lista contiene o no a un
--          elemento

estacontenido:: Eq a=>[a]->[a]->Bool
estacontenido [] list=True  --La nocion es que el vacio esta contenido en cualquier conjunto, en este caso lista
estacontenido (x:xs) list= x `elem` list && estacontenido xs list

listasiguales :: Eq a=>[a]->[a]->Bool
listasiguales lista1 lista2 = (estacontenido lista1 lista2) && (estacontenido lista1 lista2)

contiene :: Eq a => a -> [a] -> Bool
contiene x [] = False
contiene x (y:b)
    | x == y = True
    | otherwise = contiene x b

--sustForm. Función que realiza la sustitución de variables en una
--          fórmula sin renombramientos.
sustForm :: Form -> Subst -> Form
sustForm f s = error "Sin implementar."

--alphaEq. Función que dice si dos fórmulas son alpha-equivalentes.
alphaEq :: Form -> Form -> Bool
alphaEq f1 f2 = error "Sin implementar."



{-- Puntos Extra
renom :: Form -> Form
renomConj :: Form -> Form
sustFormAlpha :: Form -> Subst -> Form
--}
