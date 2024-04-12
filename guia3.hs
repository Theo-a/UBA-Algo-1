f :: Integer -> Integer
f x | x == 1 = 8
    | x == 4 = 131
    | x == 16 = 16
g :: Integer -> Integer
g x | x == 8 = 16
    | x == 16 = 4
    | x == 131 = 1
h :: Integer -> Integer
h x | x == 8 || x == 16 || x == 131 = g x
k :: Integer -> Integer
k x | x == 1 || x ==4  || x == 16 = f x
absoluto :: Integer -> Integer
absoluto x | x >= 0 = x
           | x < 0 = -x
maximoabsoluto :: Integer -> Integer -> Integer
maximoabsoluto x y | x >= y = x
                   | otherwise = y
maximo3 :: Integer -> Integer -> Integer -> Integer
maximo3 x y z | x >= maximoabsoluto y z = x
              | y >= maximoabsoluto x z = y
              | otherwise = z
algunoEs0 :: Float -> Float -> Bool
algunoEs0 x y | x == 0 || y == 0 = True
              | otherwise = False
ambosSon0 :: Float -> Float -> Bool
ambosSon0 x y | x == 0 && y == 0 = True
              | otherwise = False
sumaDistintos :: Integer -> Integer -> Integer -> Integer
sumaDistintos x y z | x /= y && y/= z = x + y + z
                    | x == y && y /= z = x + z
                    | x == z && y /= x = y + z
                    | otherwise = x
esMultiploDe :: Integer -> Integer -> Bool
esMultiploDe x y | mod x y == 0 = True
                 | otherwise = False
digitosUnidades :: Integer -> Integer
digitosUnidades x | x >= 0 = digitosUnidadesPos x
                  | x < 0 = digitosUnidadesNeg x
digitosUnidadesPos :: Integer -> Integer
digitosUnidadesPos x | x <= 9 = x
                     | otherwise = digitosUnidadesPos (x - 10)
digitosUnidadesNeg :: Integer -> Integer
digitosUnidadesNeg x | x >= -9 = x
                     | otherwise = digitosUnidadesNeg (x + 10)
digitosDecenas:: Integer -> Integer
digitosDecenas x | x >= 0 = digitosDecenasPos x
                 | x < 0 = digitosDecenasNeg x
digitosDecenasPos :: Integer -> Integer
digitosDecenasPos x | x <= 99 = quitarUnidadesPos x
                    | otherwise = digitosDecenasPos (x - 100)
quitarUnidadesPos :: Integer -> Integer
quitarUnidadesPos x | mod x 10 == 0 = x
                    |otherwise = quitarUnidadesPos (x - 1)
digitosDecenasNeg :: Integer -> Integer
digitosDecenasNeg x | x >= -99 = quitarUnidadesNeg x
                    | otherwise = digitosDecenasNeg (x + 100)
quitarUnidadesNeg :: Integer -> Integer
quitarUnidadesNeg x | mod x 10 == 0 = x
                    |otherwise = quitarUnidadesNeg (x + 1)
estanRelacionados :: Integer -> Integer -> Bool
estanRelacionados x y | div (-(x*x)) (x*y) == 0 = True
                      | otherwise = False
prodInt :: (Float, Float) -> (Float, Float) -> ((Float, Float), (Float, Float),(Float, Float),(Float, Float))
prodInt (a,b) (c,d) = ((a,c),(a,d),(b,c),(b,d))
todoMenor :: (Float,Float) -> (Float,Float) -> Bool
todoMenor (a,b) (c,d) | (a <= d) && (b <= d) = True
                      | otherwise = False
distanciaPuntos :: (Float,Float) -> (Float,Float) -> Float
distanciaPuntos (a,b) (c,d) = sqrt ((a-c)**2 + (b-d)**2)
sumaTerna :: (Integer,Integer,Integer) -> Integer
sumaTerna (a,b,c) = a + b + c 
sumarSoloMultiplos :: (Integer,Integer,Integer) -> Integer -> Integer
sumarSoloMultiplos (a,b,c) d | mod a d == 0 && mod b d == 0 && mod c d == 0 = a + b + c
                             | mod a d == 0 && mod b d == 0 && mod c d /= 0 = a + b
                             | mod a d == 0 && mod b d /= 0 && mod c d == 0 = a + c
                             | mod a d /= 0 && mod b d == 0 && mod c d == 0 = b + c
                             | mod a d == 0 && mod b d /= 0 && mod c d /= 0 = a
                             | mod a d /= 0 && mod b d == 0 && mod c d /= 0 = b
                             | mod a d /= 0 && mod b d /= 0 && mod c d == 0 = c
posPrimerPar :: (Integer, Integer, Integer) -> Integer
posPrimerPar (a,b,c) | odd a && odd b && odd c = 4
                     | even a = 0
                     | even b = 1
                     | even c = 2
crearPar :: a -> b -> (a,b)
crearPar x y = (x,y)
invertir :: (a,b) -> (b,a)
invertir (x,y) = (y,x)
todosMenores :: (Integer,Integer,Integer) -> Bool
todosMenores (a,b,c) = f2 a > g2 a && f2 b > g2 b && f2 c > g2 c
f2 :: Integer -> Integer
f2 n | n <= 7 = n*n
     | n > 7 = 2*n -1
g2 :: Integer -> Integer
g2 n | even n = div n 2
     | otherwise = 3*n +1
bisiesto :: Integer -> Bool
bisiesto x = not((mod x 4 /= 0 || mod x 100 == 0) && mod x 400 /= 0)
distanciaManhattan :: (Float,Float,Float) -> (Float,Float,Float) -> Float
distanciaManhattan (a,b,c) (d,e,f) = abs (a-d) + abs (b-e) + abs  (c-f)
comparar :: Integer -> Integer -> Integer
comparar x y | sumaUltimosDigitos x < sumaUltimosDigitos y = 1
             | sumaUltimosDigitos x > sumaUltimosDigitos y = -1
             | sumaUltimosDigitos x == sumaUltimosDigitos y = 0
sumaUltimosDigitos :: Integer -> Integer
sumaUltimosDigitos x = (mod (abs (x)) 10) + (mod (div (abs (x)) 10) 10)
{-no me dejas uar floor, pero creo que no es necesario -}
{-truncate: tira la parte entera de un numero. floor redondea. ver bien la def-}