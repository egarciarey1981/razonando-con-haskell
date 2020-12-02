-- Escriba una función que calcule el número de días de un mes, dados los valores
-- numéricos del mes y año. NOTA: Considere los años bisiestos para febrero.

import T02_E29_esBisiesto

diasDelMes :: Integer -> Integer -> Integer
diasDelMes mes año = dias !! fromIntegral (mes - 1)
  where
    dias = [31, feb, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    feb = if esBisiesto año then 29 else 28