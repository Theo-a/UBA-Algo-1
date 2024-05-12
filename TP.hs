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

descifrar :: String -> Int -> String
descifrar [] n = []
descifrar s n | esMinuscula (head s) = desplazar2 (head s) n : descifrar (tail s) n
              | otherwise = head s : descifrar (tail s) n
desplazar2 :: Char -> Int -> Char
desplazar2 c n | not (esMinuscula c) = c
               | otherwise = auxDesplazar c n (reverso['a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z'])
--el problema esta en auxDesplazar