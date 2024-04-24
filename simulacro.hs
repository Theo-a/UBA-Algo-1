relacionesValidas :: [(String,String)] -> Bool
relacionesValidas (x:xs) | (x:xs) == [] = False
                         | fst x == snd x = False
                         | longitud (x:xs) == 1 = True 
                         | pertenece x xs == True = False
                         | pertenece x2 xs == True = False
                         | otherwise = relacionesValidas (xs)
                            where x2 = (snd x, fst x)
longitud :: [t] -> Integer
longitud [] = 0
longitud (_:xs) = 1 + longitud xs
pertenece :: (Eq t) => t -> [t] -> Bool
pertenece e x | x == [] = False
              | e == head x = True
              | otherwise = pertenece e (tail x)

personas :: [(String,String)] -> [String]
personas x = auxEliminaReps (auxUneTodo x)

auxUneTodo :: [(String,String)] -> [String]
auxUneTodo [] = []
auxUneTodo (x:xs) = [fst x, snd x] ++ auxUneTodo xs
-- obs: si usas (x:xs) estas pidiendo que la lista tenga un elem al menos
auxEliminaReps :: [String] -> [String]
auxEliminaReps x | x == [] = []
                 | pertenece (head x) (tail x) = auxEliminaReps (tail x)
                 | otherwise = [head x] ++ auxEliminaReps (tail x)

amigosDe :: String -> [(String,String)] -> [String]
amigosDe s x | x == [] = []
             | pertenece2 s (head x) = [complemento s (head x)] ++ amigosDe s (tail x)
             | otherwise = amigosDe s (tail x)

pertenece2 :: (Eq t) => t -> (t,t) -> Bool
pertenece2 x y = x == fst y || x == snd y
complemento :: (Eq t) => t -> (t,t) -> t
complemento s x | s == fst x = snd x
                | s == snd x = fst x

personaConMasAmigos :: [(String,String)] -> String
personaConMasAmigos x = auxRecursion (personas x)  x

auxCantidadAmigos :: String -> [(String,String)]-> Integer
auxCantidadAmigos s x = longitud (amigosDe s x)
auxRecursion :: [String] -> [(String,String)] -> String
auxRecursion (x:xs) y | longitud (x:xs) == 1 = x
                      | auxCantidadAmigos x y >= auxCantidadAmigos (auxRecursion xs y) y = x
                      | otherwise = auxRecursion xs y



