absoluto :: Float -> Float
absoluto x | x > 0 = x
           | otherwise = 0-x

digitoUnidades :: Integer -> Integer
digitoUnidades x = mod x 10 

digitoDecenas :: Integer -> Integer
digitoUnidades x | x>9 = mod x 100

sumaUltimosDosDigitos :: Integer -> Int
sumaUltimosDosDigitos x= (mod absoluto(x) 10) + ()

posPrimerPar:: (Int, Int, Int) -> Int
posPrimerPar (x, y, z)  | mod x 2 == 0 = 1 
                        | mod y 2 == 0 = 2
                        | mod z 2 == 0 = 3
                        | otherwise = 4

type Anio = Integer
type EsBisiesto= Bool

bisiesto :: Anio ->EsBisiesto
bisiesto a = not( mod a 4 /=0 || (mod a 100 == 0 && mod a 400 /= 0))

bisiesto2 :: Anio ->EsBisiesto
bisiesto2 a = mod a 4 ==0 && mod a 100 /= 0 || mod a 400 == 0 

distanciaManhattan:: (Float, Float, Float) ->(Float, Float, Float) ->Float
distanciaManhattan (x1,y1,z1) (x2,y2,z2)= (absoluto (x1-x2)) + (absoluto (y1-y2)) + (absoluto (z1-z2))

comparar :: Integer ->Integer ->Integer
comparar x y    | sumaUltimosDosDigitos x < sumaUltimosDosDigitos y = 1
                | sumaUltimosDosDigitos x > sumaUltimosDosDigitos y = -1
                | sumaUltimosDosDigitos x = sumaUltimosDosDigitos y = 0
                where sumaUltimosDosDigitos n= digitoUnidades n + digitoDecenas n