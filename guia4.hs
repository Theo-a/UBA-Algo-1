fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)
parteEntera :: Float -> Integer
parteEntera x = floor x
esDivisible :: Integer -> Integer -> Bool
esDivisible m n |m == 0 = True 
                |abs n > abs m = False
                |otherwise = esDivisible (abs m - abs n) (abs n)
{- abs esta de mas, pero hace que funciona para negativos-}
sumaImpares :: Integer -> Integer
sumaImpares n | n == 1 = 1
              | otherwise = sumaImpares(n-1) + nEsimoImpar n
nEsimoImpar :: Integer -> Integer
nEsimoImpar n = n*2 - 1
{-
medioFact :: Integer -> Integer
medioFact n | n == 0 = 0
            | mod n 2 == 0 = medioFactPar n
            | otherwise = medioFactImpar n
medioFactPar :: Integer -> Integer
medioFactPar n | n == 0 = 1
               | otherwise = medioFactPar (n-2)*n
medioFactImpar :: Integer -> Integer
medioFactImpar n | n == 1 = 1
                 | otherwise = medioFactImpar(n-2)*n
 esta esta bien definida según la ecuación matemática porque manda el cero al cero-}
medioFact :: Integer -> Integer
medioFact n | n == 0 = 1
            | n == 1 = 1
            | otherwise = medioFact (n-2)*n
{-esta cumple pero la consigna pero no la formula-}
sumaDigitos :: Integer -> Integer
sumaDigitos n | n < 10 = n
              | otherwise = div n 100 + div (mod n 100) 10 + mod (mod n 100) 10
{-
sumaDigitos2 :: Integer -> Integer              
sumaDigitos2 n =             
-}
{-
todoDigitosIguales :: Integer -> Bool
todoDigitosIguales n =           
-}
{-
iesimoDigito :: Integer -> Integer
iesimoDigito x =
-}
