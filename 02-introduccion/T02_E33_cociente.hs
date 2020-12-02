-- Escriba una función recursiva que devuelva el cociente que se obtiene al
-- dividir dos números enteros usando sumas y restas.

cociente :: Integer -> Integer -> Integer
cociente x y
  | x < y = 0
  | otherwise = 1 + cociente (x - y) y