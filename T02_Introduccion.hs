-- Sea una lista de funciones de enteros en enteros:
-- [f1, f2, ..., fn] :: [Integer -> Integer]
--
-- Defina un operador (|>) de forma que se tenga:
-- [f1, f2, ..., fn] |> x :: [f1 x, f2 x, ..., fn x]

(|>) :: [Integer -> Integer] -> Integer -> [Integer]
[] |> _ = []
(f : fs) |> x = f x : (fs |> x)

-- Escriba una función que determine si un año es bisiesto. Un años es bisiesto si
-- es múltiplo de 4 (por ejemplo 1984). Una excepción a la regla anterior es que
-- los años múltiplos de 100 sólo son bisiestos cuando a su vez son múltiplos de
-- 400 (por ejemplo 1800 no es bisiesto, mientras que 2000 sí).

esBisiesto :: Integer -> Bool
esBisiesto año
  | esMultiploDe 400 = True
  | esMultiploDe 100 = False
  | otherwise = esMultiploDe 4
  where
    esMultiploDe n = año `mod` n == 0

-- Escriba una función que calcule el número de días de un mes, dados los valores
-- numéricos del mes y año. NOTA: Considere los años bisiestos para febrero.

diasDelMes :: Int -> Integer -> Integer
diasDelMes mes año = dias !! (mes - 1)
  where
    dias = [31, feb, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    feb = if esBisiesto año then 29 else 28

-- Escriba una función que añada un dígito a la derecha de un número entero:
-- MAIN> 3 `aLaDerechaDe` 146
-- 1463 :: Integer

aLaDerecha :: Integer -> Integer -> Integer
x `aLaDerecha` y = y * 10 + x

-- Escriba una función recursiva que devuelva el resto de la división de dos
-- enteros usando sustracciones.

resto :: Integer -> Integer -> Integer
resto x y
  | x < y = x
  | otherwise = resto (x - y) y

-- Escriba una función recursiva que devuelva el cociente que se obtiene al
-- dividir dos números enteros usando sumas y restas.

cociente :: Integer -> Integer -> Integer
cociente x y
  | x < y = 0
  | otherwise = 1 + cociente (x - y) y

-- Escriba una función recursiva que devuelva el sumatorio desde un valor entero
-- hasta otro:
-- sumDesdeHasta a b => a + (a+1) + (a+2) + ... + (b-1) + b

sumDesdeHasta :: Integer -> Integer -> Integer
sumDesdeHasta a b
  | a == b = a
  | otherwise = a + sumDesdeHasta (a + 1) b

-- Escriba una función recursiva que devuelva el producto desde un valor entero
-- hasta otro:
-- prodDesdeHasta a b => a * (a+1) * (a+2) * ... * (b-1) * b

prodDesdeHasta :: Integer -> Integer -> Integer
prodDesdeHasta a b
  | a == b = a
  | otherwise = a * prodDesdeHasta (a + 1) b
