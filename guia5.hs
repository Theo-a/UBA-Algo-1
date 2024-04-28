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
--sacarBlancosRepetidos
sacarEspaciosRepetidos :: [Char] -> [Char]
sacarEspaciosRepetidos [] = []
sacarEspaciosRepetidos (x:[]) = [x]
sacarEspaciosRepetidos (x:y:xs) | x==y && x==' ' = sacarEspaciosRepetidos (y:xs)
                                | otherwise =  x:(sacarEspaciosRepetidos (y:xs) )
contarPalabras :: [Char] -> Int
contarPalabras xs = contarEspacios (sacarEspaciosIniFin (sacarEspaciosRepetidos xs)) + 1
--saca el primer y el ultimo espacio
sacarEspaciosIniFin :: [Char] -> [Char]
sacarEspaciosIniFin [] = []
sacarEspaciosIniFin (x:xs) | x==' ' = sacarEspacioFin xs
                           | otherwise = x:(sacarEspacioFin xs) 
-- saca el ultimo esapcio
sacarEspacioFin :: [Char] -> [Char]
sacarEspacioFin [] = []
sacarEspacioFin (x:[]) | x==' ' = []
                       | otherwise = [x] 
sacarEspacioFin (x:xs) = x:(sacarEspacioFin xs)
contarEspacios :: [Char] -> Int
contarEspacios [] = 0
contarEspacios (x:xs) | x==' '= 1 + contarEspacios xs
                      | otherwise = contarEspacios xs
palabras :: [Char] -> [[Char]]
palabras xs = palabrasAux (sacarEspaciosIniFin (sacarEspaciosRepetidos xs))

palabrasAux :: [Char] -> [[Char]]
palabrasAux [] = []
palabrasAux (x:xs) = primeraPalabra (x:xs):(palabrasAux (sacarPrimeraPalabra (x:xs)))

primeraPalabra :: [Char] -> [Char]
primeraPalabra [] = []
primeraPalabra (x:xs) | x == ' ' = []
                      | otherwise = x:(primeraPalabra xs)                     
sacarPrimeraPalabra :: [Char] -> [Char]
sacarPrimeraPalabra [] = []
sacarPrimeraPalabra (x:xs) | x == ' ' = xs
                           | otherwise = sacarPrimeraPalabra xs 
palabraMasLarga :: [Char] -> [Char]
palabraMasLarga xs = palabraMasLargaAux (sacarEspaciosIniFin (sacarEspaciosRepetidos xs))

palabraMasLargaAux :: [Char] -> [Char]
palabraMasLargaAux xs | sacarPrimeraPalabra xs == [] = primeraPalabra xs
                      | length (primeraPalabra xs) >= length (palabraMasLargaAux (sacarPrimeraPalabra xs)) = primeraPalabra xs
                      | otherwise = palabraMasLargaAux (sacarPrimeraPalabra xs) 
aplanar :: [[Char]] -> [Char]
aplanar [] = []
aplanar (x:xs) = x ++ (aplanar xs)
-- obs: "a" ++ "b" -> "ab"
aplanarConBlancos :: [[Char]] -> [Char]
aplanarConBlancos x | x == [] = []
                    | otherwise = head x ++ [' '] ++ aplanarConBlancos (tail x)
aplanarConNBlancos :: [[Char]] -> Integer -> [Char]
aplanarConNBlancos x n | x == [] = []
                       | otherwise = head x ++ auxNespacios n ++ aplanarConBlancos (tail x)
auxNespacios :: Integer -> [Char]
auxNespacios n | n == 1 = [' ']
               | otherwise = ' ' : auxNespacios (n-1)
sumaAcumulada :: (Num t) => [t] -> [t]
sumaAcumulada xs | length xs == 1 = xs
                 | otherwise = sumaAcumulada (principio xs) ++[auxSumarTodos xs]
-- principio: saca ultimo elem de una lista              
auxSumarTodos :: (Num t) => [t] -> t
auxSumarTodos xs | length xs == 1 = head xs
                 | otherwise = head xs + auxSumarTodos (tail xs)
descomponerEnPrimos :: [Integer] -> [[Integer]]
descomponerEnPrimos xs | length xs == 1 = [auxFactorizar (head xs) 1]
                       | otherwise = auxFactorizar (head xs) 1 : descomponerEnPrimos (tail xs)
auxFactorizar :: Integer -> Integer -> [Integer]
auxFactorizar n i | n == 1 = []
                  | mod n (nEsimoPrimo i) == 0 = nEsimoPrimo i:auxFactorizar (div n (nEsimoPrimo i)) i 
                  | otherwise = auxFactorizar n (i+1)

nEsimoPrimo :: Integer -> Integer
nEsimoPrimo  = auxContador 1
{- sintaxis rara de vs. en realidad va: nEsimoPrimo n = auxContador 1 n -}
auxEsPrimo2 :: Integer -> Integer
auxEsPrimo2 n | esPrimo n = 1
              | otherwise = 0              
auxCuantosPrimosHasta :: Integer -> Integer
auxCuantosPrimosHasta n | n == 1 = 0
                        | otherwise = auxEsPrimo2 n + auxCuantosPrimosHasta (n-1)
auxContador :: Integer -> Integer -> Integer
auxContador q n | auxCuantosPrimosHasta q == n = q
                | otherwise = auxContador (q+1) n
esPrimo :: Integer -> Bool
esPrimo n | n == 1 = False
          | otherwise = menorDivisor n == n
menorDivisor :: Integer -> Integer
menorDivisor 1 = 1
menorDivisor n = auxDivisores n 2
auxDivisores :: Integer -> Integer -> Integer
auxDivisores n k | k == n = n
                 | mod n k == 0 = k
                 | otherwise = auxDivisores n (k+1)