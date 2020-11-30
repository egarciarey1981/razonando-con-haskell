-- Escriba una función que determine si un año es bisiesto. Un años es bisiesto si
-- es múltiplo de 4 (por ejemplo 1984). Una excepción a la regla anterior es que
-- los años múltiplos de 100 sólo son bisiestos cuando a su vez son múltiplos de
-- 400 (por ejemplo 1800 no es bisiesto, mientras que 2000 sí).

esBisiesto :: Int -> Bool
esBisiesto año
    | esMultiploDe 4 = not (esMultiploDe 100) || esMultiploDe 400
    | otherwise = False
    where esMultiploDe n = mod año n == 0