-- ! ej 1 
--1
longitud :: [tx] -> Integer
longitud [] = 0
longitud (_:xs) = 1 + longitud xs 

--2
ultimo :: [tx] -> tx
ultimo [tx] = tx
ultimo (_:xs) = ultimo xs

--3
principio :: [tx] -> [tx]
principio [tx] = []
principio (x:xs) = x:(principio xs)

--4
reverso :: [tx] -> [tx]
reverso [tx] = [tx]
reverso (x:xs) = (reverso xs) ++ [x]

-- ! ej 2

pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece e (x:xs)
    | e == x = True
    | otherwise = pertenece e xs 

todosIguales :: (Eq t) => [t] -> Bool
todosIguales [] = True
todosIguales [t] = True
todosIguales (x:xs) = (x == head xs) && (todosIguales xs)

todosDistintos :: (Eq t) => [t] -> Bool
todosDistintos [] = True
todosDistintos [t] = True
todosDistintos (x:xs) = (x /= head xs) && (todosDistintos xs)

hayRepetidos :: (Eq t) => [t] -> Bool
hayRepetidos t = not (todosDistintos t)

quitar :: (Eq t) => t -> [t] -> [t]
quitar _ [] = []
quitar e (x:xs)
    | e == x = xs
    | otherwise = [x] ++ quitar e xs

quitarTodos :: (Eq t ) => t -> [t] -> [t]
quitarTodos _ [] = []
quitarTodos e (x:xs)
    | e == x = quitarTodos e xs 
    | otherwise = [x] ++ quitarTodos e xs

eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos [] = []
eliminarRepetidos [t] = [t]
eliminarRepetidos (x:xs)
    | pertenece x xs = [x] ++ eliminarRepetidos (quitarTodos x xs)
    | otherwise = [x] ++ eliminarRepetidos xs

-- ? aux mismosElementos
perteneceAaB :: (Eq t) => [t] -> [t] -> Bool
perteneceAaB [] b = True
perteneceAaB (x:xs) b = (pertenece x b) && (perteneceAaB xs b)

mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos [tx] [ty] = tx == ty
mismosElementos a b = (perteneceAaB as bs) && (perteneceAaB bs as)
    where
        as = eliminarRepetidos a
        bs = eliminarRepetidos b

--capicua :: (Eq t) => [t] -> Bool
--capicua

-- ! HACER EL DE CAPICUA

-- ! ej 3

sumatoria :: [Integer] -> Integer
sumatoria [t] = t
sumatoria (x:xs) = x + sumatoria xs

productoria :: [Integer] -> Integer
productoria [t] = t
productoria (x:xs) = x * productoria xs

-- se devuelve el valor, no el índice
maximo :: [Integer] -> Integer
maximo [t] = t
maximo (x:xs)
    | x > (head xs) = maximo (x:(tail xs))
    | otherwise = maximo xs


minimo :: [Integer] -> Integer
minimo [t] = t
minimo (x:xs)
    | x < (head xs) = minimo (x:(tail xs))
    | otherwise = minimo xs


-- se le suma n a cada elemento de la lista
sumarN :: Integer -> [Integer] -> [Integer]
sumarN _ [] = []
sumarN n [i] = [i+n]
sumarN n (x:xs) = [x+n] ++ sumarN n xs


sumarElPrimero :: [Integer] -> [Integer]
sumarElPrimero (x:xs) = sumarN x (x:xs)

sumarElUltimo :: [Integer] -> [Integer]
sumarElUltimo s = sumarN ( ultimo s ) s

pares :: [Integer] -> [Integer]
pares [] = []
pares (x:xs)
    | mod x 2 == 0 = [x] ++ pares xs
    | otherwise = pares xs

multiplosDeN :: Integer -> [Integer] -> [Integer]
multiplosDeN _ [] = []
multiplosDeN n (x:xs)
    | mod x n == 0 = [x] ++ multiplosDeN n xs
    | otherwise = multiplosDeN n xs

ordenar :: [Integer] -> [Integer]
ordenar [] = []
ordenar a = [m] ++ ordenar (quitar m a)
    where m = minimo a

-- ! ej 4

quitarEspaciosIniciales :: [Char] -> [Char]
quitarEspaciosIniciales [] = []
quitarEspaciosIniciales (x:xs)
    | x == ' ' =  quitarEspaciosIniciales xs
    | otherwise = (x:xs)

quitarEspaciosFinales :: [Char] -> [Char]
quitarEspaciosFinales a = reverso (quitarEspaciosIniciales (reverso a))

-- que reemplaza cada subsecuencia de blancos contiguos de la primera
-- lista por un solo blanco en la segunda lista
sacarBlancosRepetidos :: [Char] -> [Char]
sacarBlancosRepetidos [] = []
sacarBlancosRepetidos (x:xs)
    | (x == ' ') &&  ((head xs) == ' ') = [x] ++ sacarBlancosRepetidos (quitarEspaciosIniciales xs)
    | otherwise = [x] ++ sacarBlancosRepetidos xs

limpiar :: [Char] -> [Char]
limpiar " " = " "
limpiar l = sacarBlancosRepetidos(quitarEspaciosFinales(quitarEspaciosIniciales l))

-- ? aux de contarPalabras
contarPalabrasAux :: [Char] -> Integer
contarPalabrasAux [t] = 1
contarPalabrasAux (x:xs)
    | x == ' ' = 1 + contarPalabrasAux xs
    | otherwise = contarPalabrasAux xs

contarPalabras :: [Char] -> Integer
contarPalabras "" = 0
-- se debe limpiar el input
contarPalabras a = contarPalabrasAux (limpiar a)


-- elimina la primera palabra (asume input limpio sin primer espacio)
eliminarPalabra :: [Char] -> [Char]
eliminarPalabra "" = ""
eliminarPalabra (x:xs)
    | x /= ' ' = eliminarPalabra xs
    | otherwise = xs

-- primero hay q limpiar el input
palabras :: [Char] -> [[Char]]
palabras l = palabrasEsp (limpiar l)

palabrasEsp :: [Char] -> [[Char]]
palabrasEsp "" = []
palabrasEsp s = [palabrasAux s] ++ palabrasEsp (eliminarPalabra s)

-- toma una lista de caracteres que inicia por un caracter y termina en un espacio 
-- y devuelve una lista con una única palabra
palabrasAux :: [Char] -> [Char]
palabrasAux [t] = [t]
palabrasAux (x:xs)
    | x /= ' ' = [x] ++ palabrasAux xs 
    | otherwise = []

palabraMasLarga :: [Char] -> [Char]
palabraMasLarga a = palabraMasLargaAux (palabras (limpiar a))

palabraMasLargaAux :: [[Char]] -> [Char]
palabraMasLargaAux [t] = t
palabraMasLargaAux (x:xs)
    | longitud x > longitud (head xs) = palabraMasLargaAux ([x]++ (tail xs))
    | otherwise = palabraMasLargaAux xs


-- ! ej 5

nat2bin :: Integer -> [Integer]
nat2bin a = reverso (nat2binInv a)

nat2binInv :: Integer -> [Integer]
nat2binInv n
    | n < 2  = [n]
    | otherwise = [mod n 2] ++ nat2binInv (div n 2)


bin2nat :: [Integer] -> Integer
bin2nat [] = 0
bin2nat (x:xs) = x*2^n + bin2nat xs
    where n = longitud (xs)