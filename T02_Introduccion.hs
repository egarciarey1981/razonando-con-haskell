-- Defina una función  que transforme una lista de dígitos en el correspondiente valor entero:
--
-- aEntro [2, 3, 4]
-- 234
--
-- Defina la función recíroca aLista
--
-- aLista 234
-- [2, 3, 4]

aEntero :: [Integer] -> Integer
aEntero [x] = x
aEntero (x : y : zs) = aEntero ((x * 10 + y) : zs)

aLista :: Integer -> [Integer]
aLista n
  | n < 10 = [n]
  | otherwise = aLista (div n 10) ++ [mod n 10]

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

-- Escriba una función variaciones que calcule el número de variaciones de m elementos
-- tomados de n en n. Use para ello la siguiente relación:
--
--                      m!
-- variaciones m n = -------
--                    (m-n)
--
-- Escriba otra alternativa que use esta otra:
--
-- variaciones m n = (m-n+1) * (m-n+2) * ... * (m-1) * m

variaciones :: Integer -> Integer -> Integer
variaciones m n = div (prodDesdeHasta 1 m) (m - n)

variaciones' :: Integer -> Integer -> Integer
variaciones' m n = prodDesdeHasta (m - n + 1) m

-- Escriba una función que calcule números combinatorios con la siguiente relación:
--
-- / m \        m!
-- |   | = -------------
-- \ n /    (m-n)! * n!
--
-- Escriba otra versión que use estas relaciones:
--
-- / m \        / m \        / m \   / m-1 \   /m-1\
-- |   | = 1    |   | = 1    |   | = |     | + |   |
-- \ 0 /        \ m /        \ n /   \ n-1 /   \ n /
combinaciones :: Integer -> Integer -> Integer
combinaciones m n = div (prodDesdeHasta 1 m) (prodDesdeHasta 1 (m - n) * prodDesdeHasta 1 n)

combinaciones' :: Integer -> Integer -> Integer
combinaciones' m n
  | n == 0 = 1
  | n == m = 1
  | otherwise = combinaciones' (m - 1) (n - 1) + combinaciones' (m - 1) n

-- Escriba una función que devuelva el i-ésimo número de la sucesión de Fibonacci.
-- Esta sucesión tiene como primer término 0, como segundo el 1, y cualquier otro
-- término se obtiene sumando los dos que le preceden: 0, 1, 1, 2, 3, 5, 8, 13...

fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

-- Escriba una función qe determine el mayor de tres números enteros.
-- Escriba otra para cuatro números.

mayor :: Integer -> Integer -> Integer -> Integer
mayor a b c = max a (max b c)

mayor' :: Integer -> Integer -> Integer -> Integer -> Integer
mayor' a b c d = max a (mayor b c d)

-- Escriba una función que tome tres números enteros y devuelva una terna con
-- los númros ordenados en orden creciente:
--
-- ordena3 10 4 7
-- (4, 7, 10)

ordena3 :: Integer -> Integer -> Integer -> (Integer, Integer, Integer)
ordena3 a b c
  | a > b = ordena3 b a c
  | b > c = ordena3 a c b
  | otherwise = (a, b, c)

-- Escriba una función que determine si un número positivo de exactamente
-- cuatro cifras es capicúa o no:
--
-- esCapicua 1221           esCapicua 12
-- True                     ERROR: número de cifras incorrecto

numCifras :: Integer -> Integer
numCifras n
  | n < 10 = 1
  | otherwise = 1 + numCifras (div n 10)

esCapicua :: Integer -> Bool
esCapicua n
  | numCifras n /= 4 = error "número de cifras incorrecto"
  | otherwise = aLista n == reverse (aLista n)