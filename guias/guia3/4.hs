-- ! EJERCICIO 4
prodInt :: (Float, Float) -> (Float, Float) -> Float
prodInt a b = fst(a)*fst(b) + snd(a)*snd(b) 

todoMenor :: (Float, Float) -> (Float, Float) -> Bool
todoMenor a b = ( (fst(a) < fst(b)) && (snd(a) < snd(b)) ) 

distanciaPuntos :: (Float, Float) -> (Float, Float) -> Float
distanciaPuntos a b = sqrt ((fst(b)-fst(a))**2 + (snd(b)-snd(a))**2)

sumaTerna :: (Int, Int, Int) -> Int
sumaTerna (a, b, c) = a + b + c

-- SUMARSOLOMULTIPLOS   
esMultiploDeN :: Int -> Int -> Bool
esMultiploDeN a b = (mod a b == 0)

funcionRara :: Int -> Bool -> Int
funcionRara a b | b == True = a
                | b == False = 0
-- SUMARSOLOMULTIPLOS

sumarSoloMultiplos :: (Int, Int, Int) -> Int -> Int
sumarSoloMultiplos (a, b, c) d = funcionRara a (esMultiploDeN a d) + funcionRara b (esMultiploDeN b d) + funcionRara c (esMultiploDeN c d)


posPrimerPar :: (Int, Int, Int) -> Int
posPrimerPar (a, b, c) | mod a 2 == 0 = 0
    | mod b 2 == 0 = 1
    | mod c 2 == 0 = 2
    | not ((mod a 2 == 0) && (mod b 2 == 0) && (mod c 2 == 0)) = 4

crearPar :: ta -> tb -> (ta, tb)
crearPar ta tb = (ta, tb)

invertir :: (ty, tx) -> (tx, ty)
invertir a = (snd(a), fst(a))
