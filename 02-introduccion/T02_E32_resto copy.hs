-- Escriba una función recursiva que devuelva el resto de la división de dos
-- enteros usando sustracciones.

resto :: Integer -> Integer -> Integer
resto x y
  | x < y = x
  | otherwise = resto (x-y) y