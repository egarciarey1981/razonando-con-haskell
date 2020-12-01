-- Sea una lista de funciones de enteros en enteros:
-- [f1, f2, ..., fn] :: [Integer -> Integer]
--
-- Defina un operador (|>) de forma que se tenga:
-- [f1, f2, ..., fn] |> x :: [f1 x, f2 x, ..., fn x]

(|>) :: [Integer -> Integer] -> Integer -> [Integer]
[] |> _ = []
(f1 : fs) |> x = f1 x : (fs |> x)