--2
parteEntera :: Float -> Int
parteEntera x
    | 0 <= x && x < 1 = 0
    | x >= 1 = parteEntera (x-1) +1

--3
esDivisible :: Int -> Int -> Bool
esDivisible a b
    | a == 0 = True
    | a < b = False
    | otherwise = esDivisible (a-b) b

--4
sumaImpares :: Int -> Int
sumaImpares n
    | n <= 0 = 0
    | otherwise = 2*n-1 + sumaImpares (n-1)

--5
medioFact :: Int -> Int
medioFact n
    | n == 0 = 1
    | n == 1 = 1
    | n == 2 = 2
    | n > 2 = n*medioFact (n-2)

--6
sumaDigitos :: Int -> Int 
sumaDigitos n 
    | n < 10 = n
    | otherwise = mod n 10 + sumaDigitos (div n 10)

--7
{-
todosDigitosIguales :: Int -> Bool 
todosDigitosIguales n
    | n < 10 = True
    | otherwise = mod n 10 == todosDigitosIguales (div n 10) 
-}

--8
iesimoDigito :: Int -> Int -> Int 
iesimoDigito n i = mod ( div n (10^((cantDigitos n)-i)) ) 10

--aux
cantDigitos :: Int -> Int
cantDigitos n
    | n < 10 = 1
    | otherwise = cantDigitos(div n 10) +1

--9
--esCapicua :: Int -> Bool

-- ej 10
-- a 
f1 :: Integer -> Integer 
f1 n 
    | n == 0 = 1
    | otherwise = 2^(n) + f1(n-1)

f2 :: Integer -> Float -> Float
f2 n q
    | n == 1 = q
    | otherwise = q^(n) + f2 (n-1) q

--hasta 2n
f3 :: Integer -> Float -> Float
f3 n q 
    | n == 1 = q+q^2
    | otherwise = q^(2*n) + q^(2*n-1) + f3 (n-1) q

f4 :: Integer -> Float -> Float
f4 n q 
    | n == 0 = 1
    | otherwise = (f3 n q ) - (f2 n q) + q^n

-- ? función auxiliar
fact :: Integer -> Integer
fact n = product [1..n]

-- ej 11
-- aproximar e
eAprox :: Integer -> Float
eAprox n 
    | n == 0 = 1 
    | otherwise = 1/fromIntegral(fact n) + eAprox(n-1)

e :: Float 
e = 2.718282

--13
sumDoble :: Integer -> Integer -> Integer
sumDoble n m 
    | n == 1 = m
    | otherwise = (sumDobleaux n m ) + sumDoble(n-1) m

sumDobleaux :: Integer -> Integer -> Integer
sumDobleaux n m 
    | m == 1 = n
    | otherwise = n^([1..m] !! fromIntegral(m-1)) + sumDobleaux n (m-1)

