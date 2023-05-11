-- No editar esta parte
main :: IO()
main = do {
  x <- readLn ;
  print(sumaPrimerosNImpares(x ::(Integer)))
  }

-- Completar la definición de la función ((usando recursión))
sumaPrimerosNImpares :: Integer -> Integer
sumaPrimerosNImpares n = (2*n + 2)*n

-- Pueden agregan las funciones que consideren necesarias
