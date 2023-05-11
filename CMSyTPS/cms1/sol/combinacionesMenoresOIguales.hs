-- No editar esta parte

main :: IO()
main = do {
  x <- readLn ;
  print(combinacionesMenoresOiguales(x ::(Integer)))
  }


combinacionesMenoresOiguales :: Integer -> Integer
combinacionesMenoresOiguales n = combinacionesMenoresOigualesUWU n n


combinacionesMenoresOigualesUWU :: Integer -> Integer -> Integer
combinacionesMenoresOigualesUWU n b | n == 1 = 1
                                    | b == 1 = sumj n 1 n 
                                    | otherwise = (sumj n b n) + combinacionesMenoresOigualesUWU n (b-1)

-- Pueden agregan las funciones que consideren necesarias
-- i, n cte
sumj :: Integer -> Integer -> Integer -> Integer
sumj n i j | j == 1 && i <= n = 1
           | j == 1 && i > n = 0
           | (i*j) <= n = 1 + sumj n i (j-1)
           | otherwise = sumj n i (j-1)