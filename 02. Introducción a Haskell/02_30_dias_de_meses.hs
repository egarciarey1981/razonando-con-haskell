-- Escriba una función que calcule el número de días de un mes, dados los valores
-- numéricos del mes y año. NOTA: Considere los años bisiestos para febrero.

esBisiesto :: Integer -> Bool
esBisiesto año
    | esMultiploDe 4 = not (esMultiploDe 100) || esMultiploDe 400
    | otherwise = False
    where esMultiploDe n = mod año n == 0

diasDelMes :: Integer -> Integer -> Integer
diasDelMes mes año 
    | not (mes `elem` [1..12]) = error "El mes debe ser un número entre 1 y 12"
    | mes `elem` [1,3,5,7,8,10,12] = 31
    | mes `elem` [4,6,9,11] = 30
    | esBisiesto año = 29
    | otherwise = 28