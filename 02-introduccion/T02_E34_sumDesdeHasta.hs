-- Escriba una función recursiva que devuelva el sumatorio desde un valor entero
-- hasta otro:
-- sumDesdeHasta a b => a + (a+1) + (a+2) + ... + (b-1) + b

sumDesdeHasta :: Integer -> Integer -> Integer
sumDesdeHasta a b
  | a < b = a + sumDesdeHasta (a+1) b
  | otherwise = b
