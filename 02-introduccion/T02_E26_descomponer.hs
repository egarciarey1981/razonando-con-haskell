-- Escriba una función:
-- descomponer :: Integer -> (Integer, Integer, Integer)
--
-- que a partir de una cantidad de segundos, devuelva un terna con las horas,
-- minutos y segundos equivalentes.

descomponer :: Integer -> (Integer, Integer, Integer)
descomponer segundos = f (0, 0, segundos)
  where
    f (horas, minutos, segundos)
      | segundos >= 60 = f (horas, minutos + 1, segundos - 60)
      | minutos >= 60 = f (horas + 1, minutos - 60, segundos)
      | otherwise = (horas, minutos, segundos)