-- Escriba una función que añada un dígito a la derecha de un número entero:
-- MAIN> 3 `aLaDerechaDe` 146
-- 1463 :: Integer

aLaDerecha :: Integer -> Integer -> Integer
aLaDerecha digito entero = (entero * 10) + digito