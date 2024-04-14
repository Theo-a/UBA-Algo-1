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
{-
sumaDigitos :: Integer -> Integer
sumaDigitos n | n < 10 = n
              | otherwise = div n 100 + div (mod n 100) 10 + mod (mod n 100) 10
-}              
{-
sumaDigitos2 :: Integer -> Integer              
sumaDigitos2 n | let q = length (show n)
               | q == 1 = h
               | otherwise = let h = div n 10^(q-1) + div (mod n 10^(q-1)) 10^(q-2)
-}
sumaDigitos :: Integer -> Integer
sumaDigitos n | n < 10 = n
               | otherwise = div n (10^(length (show n)-1)) + sumaDigitos (mod n (10^(length (show n)-1)))        
{-
length (show n) -> gives the lenght of an int, ex: 221 -> 3
-}
todosDigitosIguales :: Integer -> Bool
todosDigitosIguales n | n < 10 = True
                      | div n (10^(length (show n)-1)) /= div (mod n (10^(length (show n)-1))) 10^(length (show n)-2) = False
                      | otherwise = todosDigitosIguales (mod n 10^(length (show n)-1))
{-no se porque no funciona-}

iesimoDigito :: Integer -> Integer -> Integer
iesimoDigito n i = mod (div n 10^(toInteger (length (show n)) - i)) 10
{- el toInteger te pasa de Int a Integer, haskell me tomaba el otro como Int y no debajaba restarle i-}
esCapicua3 :: Integer -> Bool
esCapicua3 n = iesimoDigito n 1 == iesimoDigito n 3
{-
esCapicua :: Integer -> Bool
esCapicua n | 
pensa en contar desde los extremos hacie el centro y que length sea impar-}
f1 :: Integer -> Integer
f1 0 = 1
f1 n = 2^n + f1 (n-1)
f2 :: Integer -> Float -> Float
f2 1 q = q
f2 n q = q^n + f2 (n-1) q
f3 :: Integer -> Float -> Float
f3 1 q = q
f3 n q = f2 (2*n) q
f4 :: Integer -> Float -> Float
f4 0 q = 1
f4 n q = f2 (n*2) q - f2 (n-1) q
eAprox :: Integer -> Float
eAprox 0 = 1
eAprox n = 1 / fromIntegral (factorial n) + eAprox(n-1)
{-formIntegral pasa un Int a Float-}
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n*factorial(n-1)
{-
let e = eAprox 10
creo que no se pueden definir varibales globales aca, esto en el interprete si funciona
-}
raizDe2Aprox :: Integer -> Float
raizDe2Aprox n = auxiliarRaziDe2 n - 1
auxiliarRaziDe2 :: Integer -> Float
auxiliarRaziDe2 1 = 2
auxiliarRaziDe2 n = 2 + 1 / auxiliarRaziDe2 (n-1)
f5 :: Integer -> Float -> Float
f5 n 1 = f2 n 1
f5 n q = f2 n q + f5 n (q-1)
{- n es la potencia, q la base. Uso Integer por la como es f2-}
sumaPotencias :: Integer -> Integer -> Integer -> Integer
sumaPotencias q n m = f22 n q * f22 m q
f22 :: Integer -> Integer -> Integer
f22 1 q = q
f22 n q = q^n + f22 (n-1) q