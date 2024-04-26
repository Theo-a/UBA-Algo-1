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
principio2 :: [t] -> [t]
principio2 (x:xs) | longitud (x:xs) == 0 = []
                  | longitud (x:xs) == 1 = (x:xs)
                  | longitud xs == 1 = [x]
                  | otherwise = x : principio2 xs
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
hayRepetidos :: (Eq t) => [t] -> Bool
hayRepetidos x | x == [] = False
               | auxCuantasVecesX (head x) x == 2 = True
               | otherwise = hayRepetidos (tail x)
-- el enuncia es vago, puede estar mal (hayRepetidos)
auxCuantasVecesX :: (Eq t) => t -> [t] -> Integer
auxCuantasVecesX e xs | xs == [] = 0
                     | e == head xs = 1 + auxCuantasVecesX e (tail xs)
                     | otherwise = auxCuantasVecesX e (tail xs)
quitar :: (Eq t) => t -> [t] -> [t]
quitar x xs | null xs = xs
            | x == head xs = tail xs
            | otherwise = [head xs] ++ quitar x (tail xs)
quitarTodos :: (Eq t) => t -> [t] -> [t]
quitarTodos x xs | null xs = xs
                 | x == head xs = quitarTodos x (tail xs)
                 | x /= head xs = [head xs] ++ quitarTodos x (tail xs)
eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos x | x == [] = []
                    | auxCuantasVecesX (head x) x == 1 = head x:eliminarRepetidos (tail x)
                    | otherwise = eliminarRepetidos (head x:quitarTodos (head x) (tail x))
mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos x y = eliminarRepetidos x == eliminarRepetidos y
capicua :: (Eq t) => [t] -> Bool
capicua x = x == reverso x
sumatoria :: [Integer] ->Integer
sumatoria x | x == [] = 0
            | otherwise = head x + sumatoria (tail x)
productoria :: [Integer] -> Integer
productoria [x] = x
productoria xs = head xs * productoria (tail xs)
maximo :: [Integer] -> Integer
maximo x | x == [] = 0
         | longitud x == 1 = head x
         | head x >= maximo (tail x) = head x
         | otherwise = maximo (tail x)
sumarN :: Integer -> [Integer] -> [Integer]
sumarN e xs | xs == [] = []
            | otherwise = head xs + e : sumarN e (tail xs)
sumarElPrimero :: [Integer] -> [Integer]
sumarElPrimero [] = []
sumarElPrimero x = sumarN (head x) x
sumarElUltimo :: [Integer] -> [Integer]
sumarElUltimo [] = []
sumarElUltimo x = sumarN (ultimo x) x
pares :: [Integer] -> [Integer]
pares x | x == [] = []
        | mod (head x) 2 == 0 = head x:pares (tail x)
        | otherwise = pares (tail x)
multiplosDeN :: Integer -> [Integer] -> [Integer]
multiplosDeN n x | x == [] = []
                 | mod (head x) n == 0 = head x:multiplosDeN n (tail x)
                 | otherwise = multiplosDeN n (tail x)
ordenar :: [Integer] -> [Integer]
ordenar x = reverso (auxOrdenar x)
auxOrdenar :: [Integer] -> [Integer]
auxOrdenar x | x == [] = []
             | head x >= maximo (tail x) = head x: auxOrdenar (tail x)
             | otherwise = auxOrdenar (tail x ++ [head x])