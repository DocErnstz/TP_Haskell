module Variables where

usuario1 = (1, "Juan")

usuario2 = (2, "Natalia")

usuario3 = (3, "Pedro")

usuario4 = (4, "Mariela")

usuario5 = (5, "Natalia")

usuario6 = (6, "Juan")

usuario7 = (7, "Carlos")

usuario8 = (8, "Pepe")

usuario9 = (9, "Marcos")

usuario10 = (10, "Ernesto")

usuario11 = (11, "Agus")

usuario12 = (12, "Britany")

usuario13 = (13, "RobertoCarlos")

relacion1_2 = (usuario1, usuario2)

relacion1_3 = (usuario1, usuario3)

relacion1_4 = (usuario4, usuario1)

relacion2_3 = (usuario3, usuario2)

relacion2_4 = (usuario2, usuario4)

relacion3_4 = (usuario4, usuario3)

relacion6 = (usuario13, usuario1)

relacion7 = (usuario13, usuario2)

relacion8 = (usuario13, usuario3)

relacion9 = (usuario13, usuario4)

relacion10 = (usuario13, usuario5)

relacion11 = (usuario13, usuario6)

relacion12 = (usuario13, usuario7)

relacion13 = (usuario13, usuario8)

relacion14 = (usuario13, usuario9)

relacion15 = (usuario13, usuario10)

relacion16 = (usuario13, usuario11)

publicacion1_1 = (usuario1, "Este es mi primer post", [usuario2, usuario4])

publicacion1_2 = (usuario1, "Este es mi segundo post", [usuario4])

publicacion1_3 = (usuario1, "Este es mi tercer post", [usuario2, usuario5])

publicacion1_4 = (usuario1, "Este es mi cuarto post", [])

publicacion1_5 = (usuario1, "Este es como mi quinto post", [usuario5])

publicacion2_1 = (usuario2, "Hello World", [usuario4])

publicacion2_2 = (usuario2, "Good Bye World", [usuario1, usuario4])

publicacion3_1 = (usuario3, "Lorem Ipsum", [])

publicacion3_2 = (usuario3, "dolor sit amet", [usuario2])

publicacion3_3 = (usuario3, "consectetur adipiscing elit", [usuario2, usuario5])

publicacion4_1 = (usuario4, "I am Alice. Not", [usuario1, usuario2])

publicacion4_2 = (usuario4, "I am Bob", [])

publicacion4_3 = (usuario4, "Just kidding, i am Mariela", [usuario1, usuario3])

usuariosA = [usuario1, usuario2, usuario3, usuario4]

usuarios11 = [usuario1, usuario2, usuario3, usuario4, usuario5, usuario6, usuario7, usuario8, usuario9, usuario10, usuario11]

usuarios12 = [usuario1, usuario2, usuario3, usuario4, usuario5, usuario6, usuario7, usuario8, usuario9, usuario10, usuario11, usuario12]

usuariosRobertoCarlos = [usuario1, usuario2, usuario3, usuario4, usuario5, usuario6, usuario7, usuario8, usuario9, usuario10, usuario11, usuario12, usuario13]

relacionesA = [relacion1_2, relacion1_4, relacion2_3, relacion2_4, relacion3_4]

relaciones11 = [relacion6, relacion7, relacion8, relacion9, relacion10, relacion11, relacion12, relacion13, relacion14, relacion15, relacion16]

publicacionesA = [publicacion1_1, publicacion1_2, publicacion2_1, publicacion2_2, publicacion3_1, publicacion3_2, publicacion4_1, publicacion4_2]

redA = (usuariosA, relacionesA, publicacionesA)

usuariosB = [usuario1, usuario2, usuario3, usuario5]

relacionesB = [relacion1_2, relacion2_3]

publicacionesB = [publicacion1_3, publicacion1_4, publicacion1_5, publicacion3_1, publicacion3_2, publicacion3_3]

redB = (usuariosB, relacionesB, publicacionesB)

usuariosC = [usuario1, usuario2, usuario3, usuario4, usuario5, usuario6, usuario7, usuario8]
