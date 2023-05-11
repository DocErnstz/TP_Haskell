-- No editar esta parte
main :: IO()
main = do {
  x <- readLn ;
  print(prod(x ::(Integer)))
  }

{-

-}

-- Completar la definición de la función
prod :: Integer -> Integer
prod n | n == 1 = 24
       | otherwise = ( (2*n)^2 + 4*n )*prodA( 2*n - 1 )

-- Pueden agregan las funciones que consideren necesarias
prodA :: Integer -> Integer
prodA n | n == 1 = 3
        | otherwise = (n^2 + 2*n)*prodA (n-1)