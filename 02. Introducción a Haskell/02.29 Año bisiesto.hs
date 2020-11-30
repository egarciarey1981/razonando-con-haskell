{-
Escriba una función que determine si un año es bisiesto. Un años es bisiesto si
es múltiplo de 4 (por ejemplo 1984). Una excepción a la regla anterior es que
los años múltiplos de 100 sólo son bisiestos cuando a su vez son múltiplos de
400 (por ejemplo 1800 no es bisiesto, mientras que 2000 sí).
-}

esBisiesto :: Int -> Bool
esBisiesto x
    | multiplo x 100 = multiplo x 400
    | otherwise = multiplo x 4
    where multiplo x y = mod x y == 0