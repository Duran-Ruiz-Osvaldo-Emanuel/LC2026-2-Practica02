--revisar interpretacion, estsados posibles, modelos, ahi fallan 2



module Practica02 where

--Sintaxis de la logica proposicional
data Prop = Var String | Cons Bool | Not Prop
            | And Prop Prop | Or Prop Prop
            | Impl Prop Prop | Syss Prop Prop
            deriving (Eq)

instance Show Prop where 
                    show (Cons True) = "⊤"
                    show (Cons False) = "⊥"
                    show (Var p) = p
                    show (Not p) = "¬" ++ show p
                    show (Or p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
                    show (And p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
                    show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")"
                    show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")"


p, q, r, s, t, u :: Prop
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"
u = Var "u"

type Estado = [String]

--EJERCICIOS

--Ejercicio 1
variables :: Prop -> [String]
variables (Var p) = [p]
variables (Cons _) = []
variables (Not f1) = variables f1
variables (And f1 f2) = myNub (variables f2 ++ variables f1)
variables (Or f1 f2) = myNub (variables f2 ++ variables f1)
variables (Impl f1 f2) = myNub (variables f2 ++ variables f1)
variables (Syss f1 f2) = myNub (variables f2 ++ variables f1)


--Ejercicio 2
interpretacion :: Prop -> Estado -> Bool
interpretacion (Var p) estado = p `elem` estado
interpretacion (Cons a) _ = a
interpretacion (Not f1) estado = not (interpretacion f1 estado)
interpretacion (And f1 f2) estado = interpretacion f1 estado && interpretacion f2 estado
interpretacion (Or f1 f2) estado = interpretacion f1 estado || interpretacion f2 estado
interpretacion (Impl f1 f2) estado = not (interpretacion f1 estado) || interpretacion f2 estado
interpretacion (Syss f1 f2) estado = (not (interpretacion f1 estado) || interpretacion f2 estado) && (not (interpretacion f2 estado) || interpretacion f1 estado)


--Ejercicio 3
estadosPosibles :: Prop -> [Estado]
estadosPosibles f1 = conjPotencia (variables f1)

--Ejercicio 4
-- Dada una fórmula proposicional, esta devuelve la lista de todos sus modelos con estados donde la foórmula da verdadero.
modelos :: Prop -> [Estado]
modelos p =
    -- Se generan todos los estados posibles de la fórmula
    myFilter (\estado -> interpretacion p estado) --se filtran solo aquellos donde la interpretación da True
             (estadosPosibles p)

--Ejercicio 5
-- Dadas dos fórmulas proposicionales f1 y f2 devuelve True si son lógicamente equivalentes
sonEquivalentes :: Prop -> Prop -> Bool
sonEquivalentes f1 f2 =
    let
        -- Se obtienen todas las variables que aparecen en ambas fórmulas y se eliminan repetidas
        vs = myNub (variables f1 ++ variables f2)
        -- Se generan todos las interpretaciones posibles
        estados = conjPotencia vs
        diferentes = myFilter -- Se filtran los que tienen valores distintos
            (\estado ->
                interpretacion f1 estado /= interpretacion f2 estado
            ) estados
    -- Caso final cuando +no existen estados que difieran entonces son equivalentes
    in diferentes == []

--Ejercicio 6
--Recibe una fórmula proposicional y devuelve True si es verdadera en todos los estados
tautologia :: Prop -> Bool
tautologia f =
    let
        estados = estadosPosibles f  -- Se generan todos los estados posibles de la fórmula
        falsos = myFilter  -- Se filtran los estados donde la fórmula es falsa
            (\estado ->
                interpretacion f estado == False
            ) estados
    -- Caso final no hay estados donde sea falsa entonces es tautología
    in falsos == []


--Ejercicio 7
--Recibe una fórmula proposicional y devuelve True si es falsa en todos los estados
contradiccion :: Prop -> Bool
contradiccion f =
    let
        estados = estadosPosibles f -- Se generan todos los estados posibles de la fórmula
        verdaderos = myFilter -- Se filtran los estados donde la fórmula es verdadera
            (\estado ->
                interpretacion f estado == True
            ) estados
    -- Si no existen verdaderas, entonces es una contradicción
    in verdaderos == []


--Ejercicio 8
-- Funcion auxiliar que convierte una lista de formulas en una sola formula usando conjunciones
conjuncion :: [Prop] -> Prop
-- Si la lista de premisas está vacía la conjunción se considera verdadera
conjuncion [] = Cons True
-- Si la lista tiene una sola fórmula
conjuncion [p] = p
-- Si hay varias fórmulas, se toma la primera y se une con la conjunción del resto usando And
conjuncion (p:ps) = And p (conjuncion ps)

--Ejercicio 8
-- consecuenciaLogica recibe una lista de premisas y una conclusión y determina si la conclusión es consecuencia lógica de las premisas

consecuenciaLogica :: [Prop] -> Prop -> Bool
consecuenciaLogica premisas conclusion =
    -- Se construye una fórmula que representa:conjunción de todas las premisas ent conclusión
    -- sabemos que la conclusión es consecuencia lógica de las premisas
    -- si esta implicación es una tautología
    tautologia (Impl (conjuncion premisas) conclusion)

--Funcion auxiliar
conjPotencia :: [a] -> [[a]]
conjPotencia [] = [[]]
conjPotencia (x:xs) = [(x:ys) | ys <- conjPotencia xs] ++ conjPotencia xs

--Funcion auxiliar para evitar contar repetidos
myNub :: Eq a => [a] -> [a]
myNub []     = []
myNub (x:xs) = x : myNub (myFilter (/= x) xs)

--Funcion auxiliar para filtrar los elementos repetidos
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter p (x:xs)

    | p x       = x : myFilter p xs
    | otherwise = myFilter p xs