-- Defina una función:
-- aEntero :: [Integer] -> Integer
--
-- que transforme una lista de dígitos en el correspondiente valor entero:
-- MAIN> aEntero [2, 3, 4]
-- 234 :: Integer
--
-- Defina la función recíproca aLista:
-- MAIN > aLista 234
-- [2, 3, 4] :: [Integer]

aEntero :: [Integer] -> Integer
aEntero [x] = x
aEntero (x:y:zs) = aEntero ((x*10 + y) : zs)

aLista :: Integer -> [Integer]
aLista 0 = [0]
aLista 1 = [1]
aLista 2 = [2]
aLista 3 = [3]
aLista 4 = [4]
aLista 5 = [5]
aLista 6 = [6]
aLista 7 = [7]
aLista 8 = [8]
aLista 9 = [9]
aLista x = aLista (x `div` 10) ++ [x `mod` 10]