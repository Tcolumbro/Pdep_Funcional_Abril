-- Punto 1
-- Modelar las siguientes sustancias:
-- a. El hidrógeno y el oxígeno
-- b. El agua, sustancia compuesta por 2 hidrógenos y 1 oxígeno.

data Sustancia
    = Elemento Int String String String -- Valor atomico, Nombre, Letra con la que se le identifica lol, Tipo
    | Compuesto String String [(Sustancia, Int)] -- Nombre, Tipo, [Elemento, cantidad de elemento]
    deriving (Show, Eq)

hidrogeno = Elemento 1 "Hidrogeno" "H" "No metal"
oxigeno = Elemento 8 "Oxigeno" "O" "No metal"
oro = Elemento 79 "Oro" "Au" "Metal"
yodo = Elemento 53 "Yodo" "I" "Halogeno"
xenon = Elemento 54 "Xenon" "Xe" "Gas noble"

agua = Compuesto "Agua" "No metal" [(hidrogeno, 2), (oxigeno, 1)]
metal = Compuesto "Metal" "Metal" [(hidrogeno, 2), (oxigeno, 1)]

-- Punto 2
-- Poder saber si una sustancia conduce bien según un criterio. Los criterios actuales son electricidad y calor, pero podría haber más. 
-- Los metales conducen bien cualquier criterio, sean compuestos o elementos. 
-- Los elementos que sean gases nobles conducen bien a la electricidad, y los compuestos halógenos conducen bien el calor. 
-- Para el resto, no son buenos conductores.

conduceBien :: String -> Sustancia -> Bool -- True si conduce bien y False si no
conduceBien _ (Compuesto _ "Metal" _) = True
conduceBien _ (Elemento _ _ _ "Metal") = True
conduceBien "electricidad" (Elemento _ _ _ "Gas noble") = True
conduceBien "calor" (Compuesto _ "Halogeno" _) = True
conduceBien _ _ = False


--Punto 3
-- Obtener el nombre de unión de un elemento. Esto se logra añadiendo "uro" al final del nombre, pero solo si el nombre termina en consonante. Si termina en vocal, se busca hasta la última consonante y luego sí, se le concatena "uro". Por ejemplo, el nombre de unión del Flúor es "fluoruro", mientras que el nombre de unión del mercurio es "mercururo".

--dropWhile --dice vicky
-- funcion que recibe una condicion y una lista

esVocal :: Char -> Bool
esVocal 'a' = True --Yo aca puedo hacer una devolucion personalizada Ej; "Hola" ++ otro ++"como vas" --> devuelve "hola otro como vas"
esVocal 'e' = True --SI O SI, tiene que ser e, porqe si uso E. yo defini con e, por lo que en estecaso no reconoce E
esVocal 'i' = True
esVocal 'o' = True
esVocal 'u' = True
esVocal letra = False

nombreUnion :: Sustancia -> String
nombreUnion (Elemento _ nomb _ _) 
    | (esVocal.last) nomb = (init nomb) ++ "uro"
    | otherwise =  nomb ++ "uro"
--Punto 4
-- Combinar 2 nombres. Al nombre de unión del primero lo concatenamos con el segundo, agregando un "de" entre medio. Por ejemplo, si combino "cloro" y "sodio" debería obtener "cloruro de sodio".

combinar2Nombres :: Sustancia -> Sustancia -> String
combinar2Nombres elementoNomb1 (Elemento _ elementoNomb2 _ _) = (nombreUnion elementoNomb1) ++ " de " ++ elementoNomb2

--Punto 5
-- Mezclar una serie de componentes entre sí. El resultado de dicha mezcla será un compuesto. Sus componentes serán los componentes mezclados. El nombre se forma de combinar los nombres de la sustancia de cada componente. La especie será, arbitrariamente, un no metal.

--Punto 6
-- Obtener la fórmula de una sustancia:
-- para los elementos es su símbolo químico
-- para los compuestos es la concatenación de las representaciones de sus componentes y se pone entre paréntesis

-- La representación de un componente depende de la cantidad de moléculas:

-- si tiene una, entonces solo es la fórmula de su sustancia
-- si tiene más, entonces es la fórmula de su sustancia y la cantidad

-- Por ejemplo, la fórmula del agua debería ser (H2O). ¡Recuerden que una sustancia compuesta puede estar compuesta por otras sustancias compuestas!