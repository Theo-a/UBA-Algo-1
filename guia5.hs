-- lista de numeros entre -20 y 20 congruentes a 1 mod 4 creciente: [(-19),(-15)..20]
longitud :: [t] -> Integer
longitud [] = 0
longitud (_:xs) = 1 + longitud xs
ultimo :: [t] -> t
ultimo x | longitud x == 1 = head x
         | otherwise = ultimo (tail x)
principio :: [t] -> [t]
principio [] = []
principio x = init x
--se puede usar init?    
reverso :: [t] -> [t]
reverso [] = []
reverso (x:xs) = reverso xs ++ [x]
pertenece :: (Eq t) => t -> [t] -> Bool
pertenece i x | x == [] = False
              | i == head x = True
              | otherwise = pertenece i (tail x)
todosIguales :: (Eq t) => [t] -> Bool
todosIguales (x:xs) | null xs = True
                    | x /= head xs = False
                    | otherwise = todosIguales xs
todosDistintos :: (Eq t) => [t] -> Bool
todosDistintos (x:xs) | longitud (x:xs) == 1 = True
                      | not (pertenece x xs) = todosDistintos xs
                      | otherwise = False
--hayRepetidos :: (Eq t) => [t] -> Bool
quitar :: (Eq t) => t -> [t] -> [t]
quitar x xs | null xs = xs
            | x == head xs = tail xs
            | otherwise = [head xs] ++ quitar x (tail xs)
quitarTodos :: (Eq t) => t -> [t] -> [t]
quitarTodos x xs | null xs = xs
                 | x == head xs = quitarTodos x (tail xs)
                 | x /= head xs = [head xs] ++ quitarTodos x (tail xs)
