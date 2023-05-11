-- ! EJERCICIO 2
--a
absoluto :: Int -> Int
absoluto a | a >= 0 = a
           | a < 0 = -a

--b
maximoabsoluto :: Int -> Int -> Int
maximoabsoluto a b | absoluto a > absoluto b = a
                   | otherwise = b

--c 
maximo3 :: Int -> Int -> Int -> Int
maximo3 a b c | a > b && a > c = a
              | b > a && b > c = b
              | otherwise = c

--d ( sin pattern matching )
algunoEs0 :: Float -> Float -> Bool
algunoEs0 a b = a == 0 || b == 0
--d ( con pattern matching )
algunoEs0PM :: Float -> Float -> Bool
algunoEs0PM _ 0 = True
algunoEs0PM 0 _ = True
algunoEs0PM x y = False

--e sin PM
ambosSon0 :: Float -> Float -> Bool
ambosSon0 a b = a == 0 && b == 0
--e con PM
ambosSon0PM :: Float -> Float -> Bool
ambosSon0PM 0 0 = True
ambosSon0PM a b = False

--h
esMultiploDe :: Int -> Int -> Bool
esMultiploDe a b = mod a b == 0

--i 
digitoUnidades :: Int -> Int
digitoUnidades a = mod a 10

--j
digitoDecenas :: Int -> Int
digitoDecenas a = div (mod a 100) 10