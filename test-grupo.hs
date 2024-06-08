import Test.HUnit
import Solucion
import Data.List

runGrupoTests = runTestTT allTests

allTests = test [
    "esMinuscula" ~: testsEjesMinuscula,
    "letraANatural" ~: testsEjletraANatural,
    "desplazar" ~: testsEjdesplazar,
    "cifrar" ~: testsEjcifrar,
    "descifrar" ~: testsEjdescifrar,
    "cifrarLista" ~: testsEjcifrarLista,
    "frecuencia" ~: testsEjfrecuencia,
    "cifradoMasFrecuente" ~: testsEjcifradoMasFrecuente,
    "esDescifrado" ~: testsEjesDescifrado,
    "todosLosDescifrados" ~: testsEjtodosLosDescifrados,
    "expandirClave" ~: testsEjexpandirClave,
    "cifrarVigenere" ~: testsEjcifrarVigenere,
    "descifrarVigenere" ~: testsEjdescifrarVigenere,
    "peorCifrado" ~: testsEjpeorCifrado,
    "combinacionesVigenere" ~: testsEjcombinacionesVigenere
    ]


testsEjesMinuscula = test [
    "Minúscula valida" ~: esMinuscula 'j' ~?= True,
    "Mayúscula" ~: esMinuscula 'H' ~?= False,
    "Minúscula con tilde" ~: esMinuscula 'ó' ~?= False,
    "ñ minúscula" ~: esMinuscula 'ñ' ~?= False,
    "Símbolo1" ~: esMinuscula '?' ~?= False,
    "Símbolo2" ~: esMinuscula '/' ~?= False,
    "Espacio" ~: esMinuscula ' ' ~?= False,
    "Número" ~: esMinuscula '5' ~?= False
    ]

testsEjletraANatural = test [
    "Minúscula1" ~: letraANatural 'c' ~?= 2,
    "Minúscula2" ~: letraANatural 'a' ~?= 0,
    "Minúscula3" ~: letraANatural 'z' ~?= 25,
    "Minúscula4" ~: letraANatural 'm' ~?= 12
    ]

testsEjdesplazar = test [
    "No es Minúscula" ~: desplazar 'N' 3 ~?= 'N',
    "No es Minúscula2" ~: desplazar '5' 12 ~?= '5',
    "No es Minúscula3" ~: desplazar '?' 4 ~?= '?',
    "n es cero" ~: desplazar 'a' 0 ~?= 'a',
    "n chica negativa" ~: desplazar 'm' (-2) ~?= 'k',
    "n grande negativa" ~: desplazar 'a' (-100) ~?= 'e',
    "n chica positiva" ~: desplazar 'o' 3 ~?= 'r',
    "n grande positiva" ~: desplazar 'p' 34 ~?= 'x'
    ]

testsEjcifrar = test [
    "Lista vacía" ~: cifrar [] 9 ~?= [],
    "n es cero" ~: cifrar "programacion" 0 ~?= "programacion",
    "ninguno es minúscula" ~: cifrar "INTRO A LA PROGRAMACIÓN 2024!" 9 ~?= "INTRO A LA PROGRAMACIÓN 2024!",
    "todas minúsculas" ~: cifrar "computacion" 5 ~?= "htruzyfhnts",
    "todas minúsculas, n grande" ~: cifrar "computacion" 30 ~?= "gsqtyxegmsr",
    "chars variados" ~: cifrar "Intro a La Programación 2024!" 2 ~?= "Ipvtq c Lc Ptqitcocekóp 2024!"
    ]

testsEjdescifrar = test [
    "Lista vacía" ~: descifrar [] 5 ~?= [],
    "n es cero" ~: cifrar "frpsxwdflrq" 0 ~?= "frpsxwdflrq",
    "Ninguno es minúscula" ~: descifrar "INTRO A LA PROGRAMACIÓN 2024!" 9 ~?= "INTRO A LA PROGRAMACIÓN 2024!",
    "todos minúsculas" ~: descifrar "htruzyfhnts" 5 ~?= "computacion",
    "todas minúsculas, n grande" ~: descifrar "gsqtyxegmsr" 30 ~?= "computacion",
    "chars variados" ~: descifrar "Ipvtq c Lc Ptqitcocekóp 2024!" 2 ~?= "Intro a La Programación 2024!"
    ]

testsEjcifrarLista = test [
    "Lista vacía" ~: cifrarLista [] ~?= [],
    "Lista de un elemento" ~: cifrarLista ["Un Solo Elemento"] ~?= ["Un Solo Elemento"],
    "Más de un elemento1" ~: cifrarLista ["hola", "chau", "Uba"] ~?= ["hola", "dibv", "Udc"],
    "Más de un elemento2" ~: cifrarLista ["Primer Elemento", "", "INTRO","Programación"] ~?= ["Primer Elemento", "", "INTRO","Purjudpdflóq"]
    ]

testsEjfrecuencia = test [
    "Lista vacía" ~: frecuencia [] ~?= [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0],
    "Niinguna minúscula" ~: frecuencia "PROGRAMACIÓN 2024" ~?= [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0],
    "Todas minúsculas" ~: expectlistProximity (frecuencia "computacion") [9.090909,0.0,18.181818,0.0,0.0,0.0,0.0,0.0,9.090909,0.0,0.0,0.0,9.090909,9.090909,18.181818,9.090909,0.0,0.0,0.0,9.090909,9.090909,0.0,0.0,0.0,0.0,0.0],
    "Chars variados" ~: expectlistProximity (frecuencia "Intro a la Programación!") [23.529412,0.0,5.882353,0.0,0.0,0.0,5.882353,0.0,5.882353,0.0,0.0,5.882353,5.882353,11.764706,11.764706,0.0,0.0,17.647059,0.0,5.882353,0.0,0.0,0.0,0.0,0.0,0.0]
    ]

testsEjcifradoMasFrecuente = test [
    "n es cero" ~: cifradoMasFrecuente "taller" 0 ~?= ('l', 33.333336),
    "Todas minúsculas" ~: expectAnyTuplaAprox (cifradoMasFrecuente "computacion" 3) [('f', 18.181818), ('r', 18.181818)],
    "Chars variados" ~: cifradoMasFrecuente "Intro a la Programación 2024!" 2 ~?= ('c', 23.529412),
    "Misma cantidad" ~: expectAnyTuplaAprox (cifradoMasFrecuente "Tarrell" 3) [('o', 33.333336),('u', 33.333336)]
    ]

testsEjesDescifrado = test [
    "Lista s1 vacía" ~: esDescifrado "" "compu" ~?= False,
    "Lista s2 vacía" ~: esDescifrado "taller" "" ~?= False,
    "Diferente longitud" ~: esDescifrado "programacion" "computacion" ~?= False,
    "Misma longitud, dif cant de minúsculas" ~: esDescifrado "Hola" "chau" ~?= False,
    "No es cifrado" ~: esDescifrado "computacion" "hsqtyxegmsr" ~?= False,
    "Si es cifrado, todas minúsculas" ~: esDescifrado "computacion" "gsqtyxegmsr" ~?= True,
    "Si es cifrado, chars variados" ~: esDescifrado "Intro a La Programación 2024!" "Ipvtq c Lc Ptqitcocekóp 2024!" ~?= True
    ]

testsEjtodosLosDescifrados = test [
    "Lista vacía" ~: todosLosDescifrados [] ~?= [],
    "Un solo elemento" ~: todosLosDescifrados ["compu"] ~?= [],
    "Un solo elemento, sin minúsculas" ~: todosLosDescifrados ["IP"] ~?= [("IP","IP")],
    "Ninguno son cifrados entre ellos" ~: todosLosDescifrados ["computacion","fsqtyxegmsr","hola","prueba"] ~?= [],
    "Todos son cifrados entre ellos" ~: expectPermutacion(todosLosDescifrados ["computacion","gsqtyxegmsr","frpsxwdflrq"]) [("computacion","gsqtyxegmsr"),("gsqtyxegmsr","computacion"),("computacion","frpsxwdflrq"),("frpsxwdflrq","computacion"),("gsqtyxegmsr","frpsxwdflrq"),("frpsxwdflrq","gsqtyxegmsr")],
    "Algunos son cifrados1" ~: todosLosDescifrados ["c","y","test","B",""] ~?= [("c","y"),("y","c"),("B","B"),("","")],
    "Algunos son cifrados2" ~: todosLosDescifrados ["htruzyfhnts", "computacion", "mywza"] ~?= [("htruzyfhnts", "computacion"), ("computacion", "htruzyfhnts")],
    "Algunos son cifrados3" ~: expectPermutacion(todosLosDescifrados ["a","b","B","A","Hrod","hola","Hola"]) [("a","b"),("b","a"),("B","B"),("A","A"),("Hrod","Hola"),("Hola","Hrod")]
    ]

testsEjexpandirClave = test [
    "Test1" ~: expandirClave "compu" 10 ~?= "compucompu",
    "Test2" ~: expandirClave "programacion" 3 ~?= "pro",
    "Test3" ~: expandirClave "programacion" 19 ~?= "programacionprogram",
    "Test4" ~: expandirClave "clave" 23 ~?= "claveclaveclaveclavecla"
    ]

testsEjcifrarVigenere = test [
    "S vacío" ~: cifrarVigenere "" "ip" ~?= [],
    "Test1" ~: cifrarVigenere "test" "bo" ~?= "usth",
    "Test2" ~: cifrarVigenere "computacion" "computacion" ~?= "ecyeomaeqca",
    "Test3" ~: cifrarVigenere "Intro a la Programación" "clave" ~?= "Iytms l ge Prjktlmvgkón"
    ]

testsEjdescifrarVigenere = test [
    "S vacío" ~: descifrarVigenere "" "programacion" ~?= [],
    "Test1" ~: descifrarVigenere "usth" "bo" ~?= "test",
    "Test2" ~: descifrarVigenere "Iytms l ge Prjktlmvgkón" "clave" ~?= "Intro a la Programación",
    "Test3" ~: descifrarVigenere "ecyeomaeqca" "computacion" ~?= "computacion"
    ]

testsEjpeorCifrado = test [
    "S vacío" ~: expectAny(peorCifrado "" ["ip", "asdef", "ksy"]) ["ip", "asdef", "ksy"],
    "Test1" ~: peorCifrado "prueba" ["ip", "asdef", "ksy"] ~?= "asdef",
    "Test2" ~: peorCifrado "test" ["test","clave","a"] ~?= "a",
    "Test3: Una sola clave" ~: peorCifrado "programacion" ["clave"] ~?= "clave",
    "Test4: claves repetidas" ~: peorCifrado "programacion" ["clave", "ip", "clave"] ~?= "clave",
    "Test5: claves con la misma distancia" ~: expectAny(peorCifrado "la" ["ksy","ry","rt","mqw"]) ["ksy","rt","mqw"]
    ]

testsEjcombinacionesVigenere = test [
    "msjs y claves vacías" ~: combinacionesVigenere [] [] "test" ~?= [],
    "cifrado vacío" ~: combinacionesVigenere ["computacion"] ["it"] "" ~?= [],
    "cifrado sin minúsculas" ~: expectPermutacion(combinacionesVigenere ["IP","computacion","JI"] ["it","clave","test"] "IP") [("IP", "it"),("IP","clave"),("IP","test")],
    "cifrado con y sin minúsculas" ~: combinacionesVigenere ["Chau","IP","Hola"] ["clave","hguidf","ip"] "Hufi" ~?= [("Hola","hguidf")],
    "No hay combinaciones" ~: combinacionesVigenere ["computacion"] ["it"] "kdueciirqdv" ~?= [],
    "Hay combinaciones 1" ~: combinacionesVigenere ["hola", "chau"] ["a", "h"] "johb" ~?= [("chau", "h")],
    "Hay combinaciones 2" ~: combinacionesVigenere ["computacion"] ["ip"] "kdueciirqdv" ~?= [("computacion","ip")],
    "Hay varias combinaciones" ~: expectPermutacion(combinacionesVigenere ["computacion","isujygxrvzt","programacion"] ["clave","it","ip"] "kdueciirqdv") [("isujygxrvzt","clave"),("computacion","ip")] 
    ]

-- Funciones útiles

-- margetFloat(): Float
-- asegura: res es igual a 0.00001
margenFloat = 0.00001

-- expectAny (actual: a, expected: [a]): Test
-- asegura: res es un Test Verdadero si y sólo si actual pertenece a la lista expected
expectAny :: (Foldable t, Eq a, Show a, Show (t a)) => a -> t a -> Test
expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)


-- expectlistProximity (actual: [Float], expected: [Float]): Test
-- asegura: res es un Test Verdadero si y sólo si:
--                  |actual| = |expected|
--                  para todo i entero tal que 0<=i<|actual|, |actual[i] - expected[i]| < margenFloat()
expectlistProximity:: [Float] -> [Float] -> Test
expectlistProximity actual expected = esParecidoLista actual expected ~? ("expected list: " ++ show expected ++ "\nbut got: " ++ show actual)

esParecidoLista :: [Float] -> [Float] -> Bool
esParecidoLista actual expected = (length actual) == (length expected) && (esParecidoUnaAUno actual expected)

esParecidoUnaAUno :: [Float] -> [Float] -> Bool
esParecidoUnaAUno [] [] = True
esParecidoUnaAUno (x:xs) (y:ys) = (aproximado x y) && (esParecidoUnaAUno xs ys)

aproximado :: Float -> Float -> Bool
aproximado x y = abs (x - y) < margenFloat


-- expectAnyTuplaAprox (actual: CharxFloat, expected: [CharxFloat]): Test
-- asegura: res un Test Verdadero si y sólo si:
--                  para algun i entero tal que 0<=i<|expected|,
--                         (fst expected[i]) == (fst actual) && |(snd expected[i]) - (snd actual)| < margenFloat()

expectAnyTuplaAprox :: (Char, Float) -> [(Char, Float)] -> Test
expectAnyTuplaAprox actual expected = elemAproxTupla actual expected ~? ("expected any of: " ++ show expected ++ "\nbut got: " ++ show actual)

elemAproxTupla :: (Char, Float) -> [(Char, Float)] -> Bool
elemAproxTupla _ [] = False
elemAproxTupla (ac,af) ((bc,bf):bs) = sonAprox || (elemAproxTupla (ac,af) bs)
    where sonAprox = (ac == bc) && (aproximado af bf)



-- expectPermutacion (actual: [T], expected[T]) : Test
-- asegura: res es un Test Verdadero si y sólo si:
--            para todo elemento e de tipo T, #Apariciones(actual, e) = #Apariciones(expected, e)

expectPermutacion :: (Ord a, Show a) => [a] -> [a] -> Test
expectPermutacion actual expected = esPermutacion actual expected ~? ("expected list: " ++ show expected ++ "\nbut got: " ++ show actual)

esPermutacion :: Ord a => [a] -> [a] -> Bool
esPermutacion a b = (length a == length b) && (sort a == sort b)