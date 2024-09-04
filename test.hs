doubleMe :: Integer -> Integer
doubleMe x = x + x

f :: Integer -> Integer
f 1 = 8
f 4 = 131
f 16 = 16
f n = undefined

g :: Integer -> Integer
g 8 = 16
g 16 = 4
g 131 = 1

h :: Integer -> Integer
h n = f (g n)

k :: Integer -> Integer
k n = g (f n)

absoluto :: Integer -> Integer
absoluto x | x > 0 = x
           | otherwise = - x

maximoabsoluto :: Integer -> Integer -> Integer
maximoabsoluto x y | absoluto (x) >= absoluto (y) = absoluto (x)
                   | absoluto (y) >= absoluto (x) = absoluto (y)

maximo3 :: Integer -> Integer -> Integer -> Integer
maximo3 0 0 0 = 0
maximo3 x y z | (x >= y) && (x >= z) = x
              | (y >= x) && (y >= z) = y
              | (z >= x) && (z >= z) = z

algunoEs0 :: Float -> Float -> Bool
algunoEs0 0 0 = True
algunoEs0 _ 0 = True
algunoEs0 0 _ = True
algunoEs0 _ _ = False

algunoEs0Bis :: Float -> Float -> Bool
algunoEs0Bis x y | x == 0 = True
                 | y == 0 = True
                 | otherwise = False

ambosSon0 :: Float -> Float -> Bool
ambosSon0 0 0 = True
ambosSon0 _ _ = False

ambosSon0Bis :: Float -> Float -> Bool
ambosSon0Bis x y | (x == 0) && (y == 0) = True
                 | otherwise = False

mismoIntervalo :: Float -> Float -> Bool
mismoIntervalo x y | (x <= 3) && (y <= 3) = True
                   | (x > 3) && (y <= 7) && (y>3) && (x<=7)= True
                   | (x > 7) && (y >7) = True
                   | otherwise = False

sumaDistintos :: Float -> Float -> Float -> Float
sumaDistintos x y z | x == y  && y == z = 0
                    | (x/=y) && (y/= x) && (x/=z) = x+y+z
                    | (x==y) && (x/=z) && (y/=z) = y+z
                    | (x==z) && (x/=y) && (y/=z) = x+y
                    | (y==x) && (y/=z) && (x/=z) = y+z
                    | (y==z) && (x/=y) && (z/=x) = x+y

digitoUnidades :: Integer -> Integer
digitoUnidades x = mod x 10 

todoMenor :: (Float, Float) -> (Float, Float) -> Bool
todoMenor 