-- No editar esta parte
main :: IO ()
main = do
  x <- readLn
  print (sumaMenosQueMax (x :: (Int, Int, Int)))

-- Completar acá la definición de la función
sumaMenosQueMax :: (Int, Int, Int) -> Bool
sumaMenosQueMax (a, b, c) = maximo (a, b, c) > (minimo (a, b, c) + medio (a, b, c))

-- Pueden agregan las funciones que consideren necesarias
minimo :: (Int, Int, Int) -> Int
minimo (a, b, c)
    | a <= b && a <= c = a
    | b <= a && b <= c = b
    | otherwise = c

maximo :: (Int, Int, Int) -> Int
maximo (a, b, c)
    | a >= b && a >= c = a
    | b >= a && b >= c = b
    | otherwise = c

medio :: (Int, Int, Int) -> Int
medio (a, b, c)
    | a <= b && b <= c = b
    | c <= b && b <= a = b
    | b <= a && a <= c = a
    | c <= a && a <= b = a
    | b <= c && c <= a = c
    | a <= c && c <= b = c