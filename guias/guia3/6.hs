esMultiplo :: Int -> Int -> Bool
esMultiplo x y = mod x y == 0

bisiesto :: Int -> Bool
bisiesto año = False == (not (esMultiplo año 4) || (esMultiplo año 100) && not (esMultiplo año 400))