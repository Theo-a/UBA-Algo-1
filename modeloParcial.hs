import Test.HUnit
hayQueCodificar :: Char -> [(Char,Char)] -> Bool
hayQueCodificar _ [] = False
hayQueCodificar c (x:xs) | c == fst x = True
                         | otherwise = hayQueCodificar c xs
cuantasVecesHayQueCodificar :: Char -> [Char] -> [(Char,Char)] -> Integer
cuantasVecesHayQueCodificar c f x | hayQueCodificar c x == False = 0
                                  | otherwise = auxCantidadVecesCenF c f
auxCantidadVecesCenF :: Char -> [Char] -> Integer
auxCantidadVecesCenF _ [] = 0
auxCantidadVecesCenF c f | c == head f = 1 + auxCantidadVecesCenF c (tail f)
                         | otherwise = auxCantidadVecesCenF c (tail f)
laQueMasHayQueCodificar :: [Char] -> [(Char,Char)] -> Char
laQueMasHayQueCodificar f m | length f == 1 = head f
                            | cuantasVecesHayQueCodificar (head f) f m >= cuantasVecesHayQueCodificar (head (tail f)) f m = laQueMasHayQueCodificar (tail f) m
                            | otherwise = laQueMasHayQueCodificar (tail f) m
codificarFrase :: [Char] -> [(Char,Char)] -> [Char]
codificarFrase [] m = []
codificarFrase f m | hayQueCodificar (head f) m == True = [ hayQueCodificar2 (head f) m ] ++ codificarFrase (tail f) m
                   | otherwise = [head f] ++ codificarFrase (tail f) m
hayQueCodificar2 :: Char -> [(Char,Char)] -> Char
hayQueCodificar2 _ [] = ' '
hayQueCodificar2 c (x:xs) | c == fst x = snd x
                          | otherwise = hayQueCodificar2 c xs