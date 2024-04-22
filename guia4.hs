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
sumaDigitos:: Integer -> Integer
sumaDigitos n | n == 0 = 0
              | otherwise = mod n 10 + sumaDigitos (div n 10)
{-
length (show n) -> gives the lenght of an int, ex: 221 -> 3
-}
todosDigitosIguales::Int->Bool
todosDigitosIguales n | n<10 = True
                      | n<100 = rem n 10==div n 10
                      | otherwise = todosDigitosIguales (rem n 100) && todosDigitosIguales (div n 10)
iesimoDigito :: Integer -> Integer -> Integer
iesimoDigito n i = mod (div n 10^(toInteger (length (show n)) - i)) 10
{- el toInteger te pasa de Int a Integer, haskell me tomaba el otro como Int y no debajaba restarle i-}
iesimoDigito2::Integer->Integer->Integer
iesimoDigito2 n i | i == cantDigitos n = ultimoDigito n
                  | otherwise = iesimoDigito2 (sacarUltimo n) i
                  where sacarUltimo n = div n 10
                        ultimoDigito n = mod n 10
cantDigitos::Integer->Integer
cantDigitos n | n<10 =1
              | otherwise = 1+cantDigitos (sacarUltimo n)
              where sacarUltimo n = div n 10
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
{-ej 13-}
f5 :: Integer -> Float -> Float
f5 n 1 = f2 n 1
f5 n q = f2 n q + f5 n (q-1)
{- n es la potencia, q la base. Uso Integer por la como es f2-}
sumaPotencias :: Integer -> Integer -> Integer -> Integer
sumaPotencias q n m = f22 n q * f22 m q
f22 :: Integer -> Integer -> Integer
f22 1 q = q
f22 n q = q^n + f22 (n-1) q
sumaRacionales :: Float -> Float -> Float
sumaRacionales p q | p == 0 = 0
                   | q == 1 = p
                   | otherwise = p/q + sumaRacionales (p-1) q + sumaRacionales p (q-1)

menorDivisor :: Integer -> Integer
menorDivisor 1 = 1
menorDivisor n = auxDivisores n 2
auxDivisores :: Integer -> Integer -> Integer
auxDivisores n k | k == n = n
                 | mod n k == 0 = k
                 | otherwise = auxDivisores n (k+1)
esPrimo :: Integer -> Bool
esPrimo n | n == 1 = False
          | otherwise = menorDivisor n == n
sonCoprimos :: Integer -> Integer -> Bool
sonCoprimos n m | n == 1 || m == 1 = True
                | mod n (menorDivisor m) == 0 = False
                | otherwise = sonCoprimos n (div m (menorDivisor m))
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
{-demasiado codigo, debe ser mejorable-}
mayorDigitoPar :: Integer -> Integer
mayorDigitoPar n | n < 10 && even n = n
                 | even (mod n 10) && mod n 10 >= mayorDigitoPar (div n 10) = mod n 10
                 | mod n 10 < mayorDigitoPar (div n 10) = mayorDigitoPar (div n 10)
                 | otherwise = -1
{-
esSumaInicialDePrimos :: Integer -> Bool
esSumaInicialDePrimos n = auxContador2 n 1
auxEsPrimo3 :: Integer -> Integer
auxEsPrimo3 n | esPrimo n = n
              | otherwise = 0              
auxSumaDePrimosHasta :: Integer -> Integer
auxSumaDePrimosHasta n | n == 1 = 0
                       | otherwise = auxEsPrimo3 n + auxSumaDePrimosHasta (n-1)
auxContador2 :: Integer -> Integer -> Bool
auxContador2 n q | auxSumaDePrimosHasta q > n = False
                 | auxSumaDePrimosHasta q == n = True
                 | otherwise = auxContador2 n (q+1)
-}
{- funcion pero es muy larga -}
esFibonacci :: Integer -> Bool
esFibonacci n = auxEsFibonacci n 1
auxEsFibonacci:: Integer -> Integer -> Bool
auxEsFibonacci n i | n == fibonacci i = True
                   | fibonacci i > n = False
                   | otherwise = auxEsFibonacci n (i+1)
esSumaInicialDePrimos :: Integer -> Integer -> Bool
esSumaInicialDePrimos n i | n == 0 = True
                          | n < 0 = False
                          | otherwise = esSumaInicialDePrimos (n - nEsimoPrimo i) (i+1)
auxtomaValorMax :: Integer -> Integer -> Integer
auxtomaValorMax n1 n2 | n1 == n2 = n1
                      | sumaDivisores n1 2 >= sumaDivisores (n2 - 1) 2 = auxtomaValorMax n1 (n2-1)
                      | otherwise = auxtomaValorMax (n1+1) n2
sumaDivisores :: Integer -> Integer -> Integer
sumaDivisores n q | n == 1 = 1
                  | q > n = 0
                  | mod n q == 0 = q + sumaDivisores n (q+1)
                  | otherwise = sumaDivisores n (q+1)
{- q es un indice, si arranca de 1 cuneta al 1 como div, sino usar q = 2 -}
pitagoras :: Integer -> Integer -> Integer -> Integer
pitagoras n m r | n==0 = pitagorasNFijo 0 m r
                | otherwise = pitagorasNFijo n m r + pitagoras (n-1) m r
esMenorPitagoriano :: Integer->Integer->Integer->Bool
esMenorPitagoriano p q r = p^2 + q^2 <= r^2
pitagorasNFijo :: Integer -> Integer -> Integer -> Integer
pitagorasNFijo n m r | m<0 = 0
                     | esMenorPitagoriano n m r = 1 + pitagorasNFijo n (m-1) r
                     | otherwise = 0 + pitagorasNFijo n (m-1) r

