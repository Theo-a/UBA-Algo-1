import Data.Char

esMinuscula :: Char -> Bool
esMinuscula c = auxEsMinuscula c ['a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z']
auxEsMinuscula :: Char -> [Char] -> Bool
auxEsMinuscula c [] = False
auxEsMinuscula c l | c == head l = True
                   | otherwise = auxEsMinuscula c (tail l)

letraANatural :: Char -> Int
letraANatural c = auxLetraANatural c ['a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z']
auxLetraANatural :: Char -> [Char] -> Int
auxLetraANatural c l | c == head l = 0
                     | otherwise = auxLetraANatural c (tail l) + 1
-- no hace falta caso l = [] porque nunca se llegaría al mismo (la función requiere esMinuscula)

desplazar :: Char -> Int -> Char 
desplazar c n | not (esMinuscula c) = c
               | ((ord c)+n)> 122 = desplazar c (n-26)
               |((ord c)+n)<97 = desplazar c (n+26)
               | otherwise =  chr ((ord c)+n)
{-
desplazar :: Char -> Int -> Char
desplazar c n | not (esMinuscula c) = c
              | otherwise = auxDesplazar c n ['a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z']
auxDesplazar :: Char -> Int -> [Char] -> Char
auxDesplazar c n l | letraANatural c + n <= length l - 1 = auxNaturalALetra (letraANatural c + n) l
                   | otherwise = auxDesplazar c (n- length l) (reverso l)
auxNaturalALetra :: Int -> [Char] -> Char
auxNaturalALetra n l | n == 0 = head l
                     | otherwise = auxNaturalALetra (n-1) (tail l)
naturalAletra :: Int -> Char
naturalAletra n = auxNaturalALetra n ['a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z']
reverso :: [t] -> [t]
reverso [] = []
reverso (x:xs) = reverso xs ++ [x]
-}
cifrar :: String -> Int -> String
cifrar [] n = []
cifrar s n | esMinuscula (head s) = desplazar (head s) n : cifrar (tail s) n
           | otherwise = head s : cifrar (tail s) n

descifrar2 :: String -> Int -> [Char] -> String
descifrar2 [] n  l= []
descifrar2 s n l| esMinuscula (head s) = desplazar 'a' (abs (auxLetraANatural (head s) l - n)): descifrar2 (tail s) n l
                | otherwise = head s : descifrar2 (tail s) n l

descifrar :: String -> Int -> String
descifrar s n = descifrar2 s n ['a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z']

cifrarLista :: [String] -> [String]
cifrarLista  [] = []
cifrarLista ls = auxCifrarLista ls 0
auxCifrarLista :: [String] -> Int -> [String]
auxCifrarLista [] i = []
auxCifrarLista ls i = cifrar (head ls) i : auxCifrarLista (tail ls) (i+1)

frecuencia :: String -> [Float]
frecuencia s = auxFrecuencia s ['a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z']
auxFrecuencia :: String -> [Char] -> [Float]
auxFrecuencia s [] = []
auxFrecuencia s l| esMinuscula (head s) = auxCantidadChar (head l) s / fromIntegral (length s) : auxFrecuencia s (tail l)
                 | otherwise = 0 : auxFrecuencia s (tail l)
--funciona pero los decimales no son iguales al ejemplo
auxCantidadChar :: Char -> String -> Float
auxCantidadChar c [] = 0
auxCantidadChar c s | c == head s = 1 + auxCantidadChar c (tail s)
                    | otherwise = auxCantidadChar c (tail s)
{-
cifradoMasFrecuente :: String -> Int -> (Char,Float)
cifradoMasFrecuente [] n = (' ',0)
cifradoMasFrecuente s n | auxFrecChar2 (head (cifrar s n)) (cifrar s n) >= auxFrecChar2 (fst (cifradoMasFrecuente (tail s) n)) (cifrar (tail s) n) = (head (cifrar s n),auxFrecChar2 (head (cifrar s n)) (cifrar s n) )
                        | otherwise = cifradoMasFrecuente (tail s) n
no funciona bien, da mal la frecuencia
-}
cifradoMasFrecuente :: String -> Int -> (Char,Float)
cifradoMasFrecuente [] n = (' ',0)
cifradoMasFrecuente s n | auxFrecChar2 (head (cifrar s n)) (cifrar s n) == maxx (frecuencia (cifrar s n)) = (head (cifrar s n),auxFrecChar2 (head (cifrar s n)) (cifrar s n))
                        | otherwise = cifradoMasFrecuente (tail s) n
-- anda mal
auxFrecChar :: Char -> String -> Float
auxFrecChar c [] = 0
auxFrecChar c s | c == head s = 1 + auxFrecChar c (tail s)
                | otherwise = auxFrecChar c (tail s)
auxFrecChar2 :: Char -> String -> Float
auxFrecChar2 c s = auxFrecChar c s / fromIntegral (length s)
maxx :: (Ord t) => [t] -> t
maxx [x] = x 
maxx xs | head xs >= maxx (tail xs) = head xs
        | otherwise = maxx (tail xs)

esDescifrado :: String -> String -> Bool
esDescifrado s1 s2 = auxEsDecifrado s1 s2 0
auxEsDecifrado :: String -> String -> Int -> Bool
auxEsDecifrado s1 s2 n | length s1 /= length s2 = False
                       | s2 == cifrar s1 n = True
                       | n == 26 = False
                       | otherwise = auxEsDecifrado s1 s2 (n+1)

todosLosDecifrados :: [String] -> [(String,String)]
todosLosDecifrados [] = []
todosLosDecifrados (x:xs) = auxTodosLosDec x xs ++ todosLosDecifrados xs
auxTodosLosDec :: String -> [String] -> [(String,String)]
auxTodosLosDec s [] = []
auxTodosLosDec s ls | auxEsDecifrado s (head ls) 1 = (s,head ls) : auxTodosLosDec s (tail ls)
                    | otherwise = auxTodosLosDec s (tail ls)

expandirClave :: String -> Int -> String
expandirClave xs n = auxExpandirClave xs xs n
auxExpandirClave :: String -> String -> Int -> String
auxExpandirClave ys xs 0 = []
auxExpandirClave [] xs n = auxExpandirClave xs xs n
auxExpandirClave ys xs n = [head ys] ++ auxExpandirClave (tail ys) xs (n-1)

-- codigo robado
desplazar3 :: Char -> Int -> Char 
desplazar3 c n | not (esMinuscula c) = c
               | ((ord c)+n)> 122 = desplazar3 c (n-26)
               |((ord c)+n)<97 = desplazar3 c (n+26)
               | otherwise =  chr ((ord c)+n)

todosLosDescifrados :: [String] -> [(String, String)]
todosLosDescifrados [] = []
todosLosDescifrados (x:xs) =   todosLosDescifrados xs ++ todosLosDescifradosAux x (xs)

todosLosDescifradosAux :: String-> [String] -> [(String, String)]
todosLosDescifradosAux _ [] = []
todosLosDescifradosAux c (x:xs) | (esDescifrado c x) = todosLosDescifradosAux c xs ++ [(c, x) , (x, c)] 
                                | otherwise = todosLosDescifradosAux c xs

cifrarVigenere :: String -> String -> String
cifrarVigenere ls clave = cifrarVigenereAux ls  (expandirClave clave (length (ls)))

cifrarVigenereAux :: String -> String -> String
cifrarVigenereAux [] [] = []
cifrarVigenereAux (x:xs) (n:ns) = [desplazar3 x ((ord n)-97)] ++ cifrarVigenereAux xs ns

descifrarVigenere :: String -> String -> String
descifrarVigenere ls clave = descifrarVigenereAux ls  (expandirClave clave (length (ls)))

descifrarVigenereAux :: String -> String -> String
descifrarVigenereAux [] [] = []
descifrarVigenereAux (x:xs) (n:ns) = [desplazar3 x ((ord n - 97)* (-1))] ++ descifrarVigenere xs ns

-- fin de codigo robado

peorCifrado :: String -> [String] -> String
peorCifrado s (x:xs) | auxDistanciaSec s (cifrarVigenere s x) >= auxDistanciaSec s (peorCifrado s xs) = x
                     | otherwise = peorCifrado s xs

auxDistanciaSec :: String -> String -> Int
auxDistanciaSec [] [] = 0
auxDistanciaSec s1 s2 = abs (letraANatural (head s1) - letraANatural (head s2)) + auxDistanciaSec (tail s1) (tail s2)

combinacionesVigenere :: [String] -> [String] -> String -> [(String,String)]
combinacionesVigenere [] claves cifrado = []
combinacionesVigenere msjs claves cifrado = auxCombVig (head msjs) claves cifrado ++ combinacionesVigenere (tail msjs) claves cifrado

auxCombVig :: String -> [String] -> String -> [(String,String)]
auxCombVig msj [] cifrado = []
auxCombVig msj claves cifrado | cifrarVigenere msj (head claves) == cifrado = (msj,head claves) : auxCombVig msj (tail claves) cifrado
                              | otherwise = auxCombVig msj (tail claves) cifrado