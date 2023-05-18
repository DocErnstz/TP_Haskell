module Solucion where

type Usuario = (Integer, String) -- (id, nombre)

type Relacion = (Usuario, Usuario) -- usuarios que se relacionan

type Publicacion = (Usuario, String, [Usuario]) -- (usuario que publica, texto publicacion, likes)

type RedSocial = ([Usuario], [Relacion], [Publicacion])

--------------------------------------------------- Funciones basicas

-- Devuelve el conjunto de usuarios.
usuarios :: RedSocial -> [Usuario]
usuarios (us, _, _) = us

-- Devuelve el conjunto de relaciones.
relaciones :: RedSocial -> [Relacion]
relaciones (_, rs, _) = rs

-- Devuelve el conjunto de publicaciones.
publicaciones :: RedSocial -> [Publicacion]
publicaciones (_, _, ps) = ps

-- Devuelve el n ́umero de identificaci ́on de un usuario.
idDeUsuario :: Usuario -> Integer
idDeUsuario (id, _) = id

-- Devuelve el nombre de un usuario.
nombreDeUsuario :: Usuario -> String
nombreDeUsuario (_, nombre) = nombre

-- Devuelve el usuario de una publicaci on.
usuarioDePublicacion :: Publicacion -> Usuario
usuarioDePublicacion (u, _, _) = u

-- Devuelve el conjunto de usuarios que le dieron me gusta a una publicaci on.
likesDePublicacion :: Publicacion -> [Usuario]
likesDePublicacion (_, _, us) = us

-- Devuelve la longitud de una lista.
longitud :: [t] -> Integer
longitud [] = 0
longitud (x : xs) = 1 + longitud xs

-- Dada una lista de elementos y un elemento, devuelve la lista sin la primera aparición de ese elemento.
quitar :: (Eq t) => t -> [t] -> [t]
quitar e [] = []
quitar e (x : xs)
  | e == x = xs
  | elem e xs = x : (quitar e xs)
  | otherwise = (x : xs)

-- Dada una red y un usuario, devuelve una lista con todos los usuarios con los que se relaciona el usuario en esa red.
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe red u = amigosDeEnListaDeRelaciones (relaciones red) u

-- Dada una lista de relaciones y un usuario, devuelve una lista con todos los usuarios con los que se relaciones el usuario.
amigosDeEnListaDeRelaciones :: [Relacion] -> Usuario -> [Usuario]
amigosDeEnListaDeRelaciones [] u = []
amigosDeEnListaDeRelaciones rels u
  | u1 == u = u2 : amigosDeEnListaDeRelaciones (tail (rels)) u
  | u2 == u = u1 : amigosDeEnListaDeRelaciones (tail (rels)) u
  | otherwise = amigosDeEnListaDeRelaciones (tail (rels)) u
  where
    u1 = fst (head (rels))
    u2 = snd (head (rels))

-- Dada una red social, devuelve el usuario con más amigos en la misma.
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos red = compararCantidadDeAmigos red (usuarios red)

-- Dada una red social y una lista de usuarios, devuelve el usuario con más relaciones (más amigos) en la lista de usuarios.
compararCantidadDeAmigos :: RedSocial -> [Usuario] -> Usuario
compararCantidadDeAmigos red (x : []) = x
compararCantidadDeAmigos red us
  | cantidadDeAmigos red u1 <= cantidadDeAmigos red u2 = compararCantidadDeAmigos red (tail (us))
  | otherwise = compararCantidadDeAmigos red (quitar u2 us)
  where
    u1 = head (us)
    u2 = head (tail (us))

-- Dada una red y un usuario, devuelve la longitud de la lista de amigos del usuario en esa red.
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos red u = fromInteger (longitud (amigosDe red u))

-- Dada una red, valida si hay un usuario con más de un millón de amigos.
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos red = (cantidadDeAmigos red (usuarioConMasAmigos red)) > 10

-- Dada una red y un usuario, devuelve la lista de todas las publicaciones del usuario en esa red.
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe red u = filtrarPublicacionesPorUsuario u (publicaciones red)

-- Dado un usuario y una lista de publicaciones, devuelve la lista de todas las publicaciones que sean del usuario en la lista de publicaciones.
filtrarPublicacionesPorUsuario :: Usuario -> [Publicacion] -> [Publicacion]
filtrarPublicacionesPorUsuario _ [] = []
filtrarPublicacionesPorUsuario u pubs
  | u == upub = pub : (filtrarPublicacionesPorUsuario u pubsres)
  | otherwise = filtrarPublicacionesPorUsuario u pubsres
  where
    pub = head (pubs)
    pubsres = tail (pubs)
    upub = usuarioDePublicacion pub

-- Dada una red y un usuario, devuelve la lista de todas las publicaciones a las cuales el usuario les puso like en esa red.
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA red u = filtrarPublicacionesConLikeDeUsuario u (publicaciones red)

-- Dado un usuario y una lista de publicaciones, devuelve la lista de todas publicaciones a las que el usuarios les puso like en la lista de publicaciones.
filtrarPublicacionesConLikeDeUsuario :: Usuario -> [Publicacion] -> [Publicacion]
filtrarPublicacionesConLikeDeUsuario _ [] = []
filtrarPublicacionesConLikeDeUsuario u pubs
  | (elem u likespub) = pub : (filtrarPublicacionesConLikeDeUsuario u pubsres)
  | otherwise = filtrarPublicacionesConLikeDeUsuario u pubsres
  where
    pub = head (pubs)
    pubsres = tail (pubs)
    likespub = likesDePublicacion pub

-- Dada una red y dos usuarios verifica si ambos le dieron like a las mismas publicaciones.
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones red u1 u2 = mismosElementos (publicacionesQueLeGustanA red u1) (publicacionesQueLeGustanA red u2)

-- Valida que ambas listas tienen la misma longitud y los mismos elementos, sin importar las repeticiones.
mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos [] [] = True
mismosElementos l1 l2 = (longitud l1 == longitud l2) && (todoElemPertenece l1 l2) && (todoElemPertenece l2 l1)

-- Valida que todo elemento de la primer lista pertenece a la segunda.
todoElemPertenece :: (Eq t) => [t] -> [t] -> Bool
todoElemPertenece [] b = True
todoElemPertenece (x : xs) b = (elem x b) && (todoElemPertenece xs b)

-- Dada una red y un usuario, verifica si hay alguien en la lista de usuarios de la red que le haya dado like a todas las publicaciones en la red del usuario. 
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel red u = algunoLeDioLikeATodasPublicaciones red (publicacionesDe red u) (usuarios red) 

-- Dada una red, una lista de publicaciones y una lista de usuarios, verifica si toda publicación de la lista de publicaciones pertenece a la lista de publicaciones que le gustan a algún usuario en la lista de usuarios.
algunoLeDioLikeATodasPublicaciones :: RedSocial -> [Publicacion] -> [Usuario] -> Bool
algunoLeDioLikeATodasPublicaciones _ _ [] = False
algunoLeDioLikeATodasPublicaciones _ [] _ = False
algunoLeDioLikeATodasPublicaciones red pubs us = (sonListasIguales pubs (publicacionesQueLeGustanA red u)) || (algunoLeDioLikeATodasPublicaciones red pubs usres)
                                                 where u = head(us)
                                                       usres = tail(us)


-- describir qué hace la función: .....
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos red u1 u2 = pertenecenAlMismoGrupo u1 u2 grupos
      where grupos = agrupar [[]] (relaciones red)


-- dada una lista de lista de usuarios o lista de "grupos" toma una relación y si uno de los usuarios de la rel
-- pertenece a algún grupo existente, añade a ambos usuarios de la relación al grupo
-- en otro caso crea un nuevo grupo

-- dicho de otra forma
-- dada una lista de listas de usuarios o lista de "grupos" añade a todo usuario al grupo con el cual se relaciona
agrupar :: [[Usuario]] -> [Relacion] -> [[Usuario]]
agrupar _ [] = []
agrupar grupos rels = agregarAlGrupo ( agrupar grupos relRes ) rel 
      where (rel:relRes) = rels

-- agrega un usuario al grupo al cual pertenece (se relaciona con algún miembro), sino crea un nuevo grupo
agregarAlGrupo :: [[Usuario]] -> Relacion -> [[Usuario]]
agregarAlGrupo [[]] (u1,u2) = [[u1]++[u2]]
agregarAlGrupo [] (a, b) = [[a]++[b]]
agregarAlGrupo grupos rel
      | elem u1 grup1 = [grup1++[u2]]++grupRes
      | elem u2 grup1 = [grup1++[u1]]++grupRes
      | otherwise = [grup1] ++ agregarAlGrupo grupRes rel
      where
            (grup1:grupRes) = grupos
            (u1, u2) = rel

pertenecenAlMismoGrupo :: (Eq t) => t -> t -> [[t]] -> Bool
pertenecenAlMismoGrupo u1 u2 [] = False
pertenecenAlMismoGrupo u1 u2 (grup1:grupRes) = ((elem u1 grup1) && (elem u2 grup1)) || pertenecenAlMismoGrupo u1 u2 grupRes



main = do
  -- case1 el usuario no tiene publicaciones
  print(existeSecuenciaDeAmigos ([usuario1,usuario2], [(usuario1,usuario2)], []) usuario1 usuario2)
  -- 
  print(existeSecuenciaDeAmigos ([usuario1,usuario2], [], []) usuario1 usuario2)


sonListasIguales :: Eq a => [a] -> [a] -> Bool
sonListasIguales [] [] = True
sonListasIguales [] _ = False
sonListasIguales _ [] = False
sonListasIguales (x:xs) (y:ys) = x == y && sonListasIguales xs ys

redC = ([usuario1, usuario2], relacionesA, [(usuario2, "Este es mi primer post", [usuario2]), (usuario2, "Este es mi segundo post", [usuario1])])



