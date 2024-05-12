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
              | otherwise = auxDesplazar c n ['a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z']
auxDesplazar :: Char -> Int -> [Char] -> Char
auxDesplazar c n l | letraANatural c + n <= length l - 1 = auxNaturalALetra (letraANatural c + n) l
                   | otherwise = auxDesplazar c (n- length l) (reverso l)
auxNaturalALetra :: Int -> [Char] -> Char
auxNaturalALetra n l | n == 0 = head l
                     | otherwise = auxNaturalALetra (n-1) (tail l)
reverso :: [t] -> [t]
reverso [] = []
reverso (x:xs) = reverso xs ++ [x]

cifrar :: String -> Int -> String
cifrar [] n = []
cifrar s n | esMinuscula (head s) = desplazar (head s) n : cifrar (tail s) n
           | otherwise = head s : cifrar (tail s) n

descifrar :: String -> Int -> [Char] -> String
descifrar [] n  l= []
descifrar s n l| esMinuscula (head s) = desplazar 'a' (abs (auxLetraANatural (head s) l - n)): descifrar (tail s) n l
               | otherwise = head s : descifrar (tail s) n l

descifrar2 :: String -> Int -> String
descifrar2 s n = descifrar s n ['a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z']

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
                    