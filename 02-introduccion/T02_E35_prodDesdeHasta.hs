-- Escriba una función recursiva que devuelva el producto desde un valor entero
-- hasta otro:
-- prodDesdeHasta a b => a * (a+1) * (a+2) * ... * (b-1) * b

prodDesdeHasta :: Integer -> Integer -> Integer
prodDesdeHasta a b
  | a < b = a * prodDesdeHasta (a+1) b
  | otherwise = b