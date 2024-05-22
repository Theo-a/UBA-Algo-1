module Solucion where
import Data.Char
-- No se permite agrear nuevos imports
-- Sólo está permitido usar estas funciones:
-- https://campus.exactas.uba.ar/pluginfile.php/557895/mod_resource/content/1/validas_tp.pdf


-- Completar!
-- Nombre de grupo: { lukitas un gusto }
-- Integrante1: { 46502947,Badii Marina }
-- Integrante2: { 44613453,Banquero Albarracin Theo }
-- Integrante3: { 43722634,D'Alessio María José }
-- Integrante4: { 42997261,Fassl Agustin Lucas }
-- Integrantes que abandonaron la materia: {En caso que haya abandonado la materia algún
                        -- integrante, completar con los dni y apellidos, sino dejar vacío}

-- EJ 1
esMinuscula :: Char -> Bool
esMinuscula c = ord c >= 97 && ord c <= 122

-- EJ 2
letraANatural :: Char -> Int
letraANatural c = ord c - 97

-- EJ 3
desplazar :: Char -> Int -> Char
desplazar c n | not (esMinuscula c) = c
              | (ord c + n) > 122 = desplazar c (n - 26)
              | (ord c + n) < 97 = desplazar c (n + 26)
              | otherwise = chr (ord c + n)

-- EJ 4
cifrar :: String -> Int -> String
cifrar [] _ = []
cifrar (letra:palabra) n | esMinuscula letra = desplazar letra n : cifrar palabra n
                         | otherwise = letra : cifrar palabra n

-- EJ 5
descifrar :: String -> Int -> String
descifrar [] _ = []
descifrar (letra:frase) n | esMinuscula letra = desplazar letra (-n) : descifrar frase n
                          | otherwise = letra : descifrar frase n

-- EJ 6
cifrarLista :: [String] -> [String]
cifrarLista [] = []
cifrarLista ls = auxCifrarLista ls 0

auxCifrarLista :: [String] -> Int -> [String]
auxCifrarLista [] _ = []
auxCifrarLista (l:ls) i = cifrar l i : auxCifrarLista ls (i+1)

-- EJ 7
frecuencia :: String -> [Float]
frecuencia s = porcentajeMinusculas s (ord 'a')

porcentajeMinusculas :: String -> Int -> [Float]
porcentajeMinusculas _ letra | letra > 122 = []
porcentajeMinusculas s letra | cantidadMinusculas s == 0 = 0:porcentajeMinusculas s (letra + 1)
                             | otherwise = ((cantidadChar s (chr letra) / cantidadMinusculas s) * 100): porcentajeMinusculas s (letra + 1)

cantidadChar :: String -> Char -> Float
cantidadChar [] _ = 0
cantidadChar (letra:frase) c | letra == c = 1 + cantidadChar frase c
                             | otherwise = cantidadChar frase c

cantidadMinusculas :: String -> Float
cantidadMinusculas [] = 0
cantidadMinusculas (letra:frase) | esMinuscula letra = 1 + cantidadMinusculas frase
                                 | otherwise = cantidadMinusculas frase

-- Ej 8
cifradoMasFrecuente :: String -> Int -> (Char, Float)
cifradoMasFrecuente s n = mayorFrecuencia (frecuencia (cifrar s n)) [(ord 'a'),(ord 'b')]

mayorFrecuencia :: [Float] -> [Int] -> (Char, Float)
mayorFrecuencia [a] [min1,min2] = (chr min1,a)
mayorFrecuencia (a:b:c) [min1,min2] | a >= b = mayorFrecuencia (a:c) [min1,min2 + 1]
                                    | otherwise = mayorFrecuencia (b:c) [min2, min2 + 1]

-- EJ 9
esDescifrado :: String -> String -> Bool
esDescifrado s1 s2 | length s1 /= length s2 = False
                   | cantidadMinusculas s1 /= cantidadMinusculas s2 = False
                   | otherwise = auxEsDecifrado s1 s2 0

auxEsDecifrado :: String -> String -> Int -> Bool
auxEsDecifrado s1 s2 n | n == 26 = False
                       | s2 == cifrar s1 n = True
                       | otherwise = auxEsDecifrado s1 s2 (n+1)

-- EJ 10
todosLosDescifrados :: [String] -> [(String, String)]
todosLosDescifrados [] = []
todosLosDescifrados (x:xs) | cantidadMinusculas x == 0 = [(x,x)] ++ todosLosDescifrados xs
                           | otherwise = todosLosDescifradosAux x xs ++ todosLosDescifrados xs

todosLosDescifradosAux :: String-> [String] -> [(String, String)]
todosLosDescifradosAux _ [] = []
todosLosDescifradosAux c (x:xs) | esDescifrado c x = [(c, x) , (x, c)] ++ todosLosDescifradosAux c xs
                                | otherwise = todosLosDescifradosAux c xs

-- EJ 11
expandirClave :: String -> Int -> String
expandirClave clave n = expandirClaveAux clave n clave

expandirClaveAux :: String -> Int -> String -> String
expandirClaveAux _ 0 _ = []
expandirClaveAux [] n clave = expandirClaveAux clave n clave
expandirClaveAux (x:xs) n clave = x : expandirClaveAux xs (n-1) clave

-- EJ 12
cifrarVigenere :: String -> String -> String
cifrarVigenere s clave = cifrarVigenereAux s (expandirClave clave (length s)) 1

cifrarVigenereAux :: String -> String -> Int -> String
cifrarVigenereAux [] [] _ = []
cifrarVigenereAux (x:xs) (n:ns) i = desplazar x ((ord n - 97) * i) : cifrarVigenereAux xs ns i

-- EJ 13
descifrarVigenere :: String -> String -> String
descifrarVigenere ls clave = cifrarVigenereAux ls (expandirClave clave (length ls)) (-1)

-- EJ 14
peorCifrado :: String -> [String] -> String
peorCifrado _ [clave] = clave
peorCifrado s (clave1:clave2:claves) | auxDistanciaSec s (cifrarVigenere s clave1) <= auxDistanciaSec s (cifrarVigenere s clave2) = peorCifrado s (clave1:claves)
                                     | otherwise = peorCifrado s (clave2:claves)

auxDistanciaSec :: String -> String -> Int
auxDistanciaSec [] [] = 0
auxDistanciaSec (s1:s1s) (s2:s2s) | (letraANatural s1 - letraANatural s2) < 0 = - (letraANatural s1 - letraANatural s2) + auxDistanciaSec s1s s2s
                                  | otherwise = (letraANatural s1 - letraANatural s2) + auxDistanciaSec s1s s2s

-- EJ 15
combinacionesVigenere :: [String] -> [String] -> String -> [(String, String)]
combinacionesVigenere [] _ _ = []
combinacionesVigenere (msj:msjs) claves cifrado = auxCombVig msj claves cifrado ++ combinacionesVigenere msjs claves cifrado

auxCombVig :: String -> [String] -> String -> [(String,String)]
auxCombVig _ [] _ = []
auxCombVig msj (clave:claves) cifrado | cifrarVigenere msj clave == cifrado = (msj,clave) : auxCombVig msj claves cifrado
                                      | otherwise = auxCombVig msj claves cifrado

