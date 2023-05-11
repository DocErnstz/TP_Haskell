-- ! EJERCICIO 5
todosMenores :: (Int, Int, Int) -> Bool
todosMenores (a, b, c) = (f a > g a) && (f b > g b) && ( f c > g c )

f :: Int -> Int
f x | mod x 2 == 0 = div x 2
    | otherwise = 3*x + 1

g :: Int -> Int
g n | n <= 7 = n^2
    | otherwise = 2*n -1