
import Test.HUnit
import SolucionOriginal
import Variables

-- Casos de Testo

run = runTestTT estaRobertoCarlosTests
run2 = runTestTT publicacionesDeTests
run3 = runTestTT publicacionesQueLeGustanATests
run4 = runTestTT lesGustanLasMismasPublicacionesTests
run5 = runTestTT tieneUnSeguidorFielTests
run6 = runTestTT existeSecuenciaDeAmigosTests 

estaRobertoCarlosTests = test [
    "Caso 1: Hay solo 11 usuarios" ~: (estaRobertoCarlos (usuariosA, relacionesA, publicacionesA)) ~?= False,
    "Caso 2: Hay menos de 11 relaciones" ~: (estaRobertoCarlos (usuarios12, relacionesA, publicacionesA)) ~?= False,
    "Caso 3: Hay 10 relaciones" ~: (estaRobertoCarlos (usuariosRobertoCarlos, relaciones11, publicacionesA)) ~?= True
    ]

publicacionesDeTests = test [
    "Caso 1: No hay publicaciones" ~: (publicacionesDe (usuariosA, relacionesA, []) usuario1) ~?= [],
    "Caso 2: Hay publicaciones del usuario" ~: (publicacionesDe (usuariosA, relacionesA, publicacionesB) usuario1) ~?= [((1,"Juan"),"Este es mi tercer post",[(2,"Natalia"),(5,"Natalia")]),((1,"Juan"),"Este es mi cuarto post",[]),((1,"Juan"),"Este es como mi quinto post",[(5,"Natalia")])],
    "Caso 3: Hay publicaciones pero ninguna del usuario" ~: (publicacionesDe (usuariosA, relacionesA, publicacionesB) usuario13) ~?= []
    ]

publicacionesQueLeGustanATests = test [
    "Caso 1: Hay publicaciones con likes del usuario " ~: (publicacionesQueLeGustanA (usuariosA, relacionesA, publicacionesA) usuario2) ~?= [((1,"Juan"),"Este es mi primer post",[(2,"Natalia"),(4,"Mariela")]),((3,"Pedro"),"dolor sit amet",[(2,"Natalia")]),((4,"Mariela"),"I am Alice. Not",[(1,"Juan"),(2,"Natalia")])],
    "Caso 2: No Hay publicaciones donde el usuario haya dado like" ~: (publicacionesQueLeGustanA (usuariosA, relacionesA, [(usuario1, "Este es mi primer post", [usuario13])]) usuario2) ~?= [],
    "Caso 3: No hay publicaciones" ~: (publicacionesQueLeGustanA (usuariosA, relacionesA, [(usuario1, "Este es mi primer post", [usuario13])]) usuario2) ~?= []
    ]


lesGustanLasMismasPublicacionesTests = test [
    "Caso 1: Hay ninguno le gusto nada" ~: (lesGustanLasMismasPublicaciones (usuariosA, relacionesA, [(usuario1, "Este es mi primer post", [])]) usuario2 usuario1) ~?= True,
    "Caso 2: Les gusto lo mismo" ~: (lesGustanLasMismasPublicaciones (usuariosA, relacionesA, [(usuario1, "Este es mi primer post", [usuario2, usuario1])]) usuario2 usuario1) ~?= True,
    "Caso 3: No tienen likes en comun" ~: (lesGustanLasMismasPublicaciones (usuariosA, relacionesA, [(usuario2, "Este es mi primer post", [usuario1]), (usuario1, "Este es mi segundo post", [usuario2])]) usuario2 usuario1) ~?= False,
    "Caso 4: Algunos de sus likes son en comun" ~: (lesGustanLasMismasPublicaciones (usuariosA, relacionesA, [(usuario2, "Este es mi primer post", [usuario1]), (usuario2, "Este es mi segundo post", [usuario2, usuario1])]) usuario2 usuario1) ~?= False
    ]


tieneUnSeguidorFielTests = test [
    "Caso 1: el usuario no tiene publicaciones" ~: (tieneUnSeguidorFiel ([usuario1, usuario2], relacionesA, [(usuario2, "Este es mi primer post", []), (usuario2, "Este es mi segundo post", [])]) usuario1) ~?= False,
    "Caso 2: algun usuario le dio me gusta a todas las publicaciones" ~: (tieneUnSeguidorFiel (usuariosA, relacionesA, [(usuario1, "Este es mi primer post", [usuario2]), (usuario1, "Este es mi segundo post", [usuario2])]) usuario1) ~?= True,
    "Caso 3: ningun usuario le dio me gusta a todas las publicaciones" ~: (tieneUnSeguidorFiel (usuariosA, relacionesA, [(usuario1, "Este es mi primer post", []), (usuario1, "Este es mi segundo post", [usuario2])]) usuario1) ~?= False
    ]


existeSecuenciaDeAmigosTests = test [
    "Caso 1: Hay solo 2 usuarios y estan relacionados" ~: (existeSecuenciaDeAmigos ([usuario1,usuario2], [(usuario1,usuario2)], []) usuario1 usuario2) ~?= True,
    "Caso 2: Hay solo 2 usuarios y no estan relacionados" ~: (existeSecuenciaDeAmigos ([usuario1,usuario2], [], []) usuario1 usuario2) ~?= False
    ]

