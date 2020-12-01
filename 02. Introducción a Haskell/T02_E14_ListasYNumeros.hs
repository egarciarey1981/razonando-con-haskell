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
aEntero [] = 0
aEntero (x : xs) = x * (10 ^ length xs) + aEntero xs

aLista :: Integer -> [Integer]
aLista x
  | x < 10 = [x]
  | otherwise = aLista (x `div` 10) ++ [x `mod` 10]