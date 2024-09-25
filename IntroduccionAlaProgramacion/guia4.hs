-- Fibonacci function
fibonacci :: Int -> Int
fibonacci x
  | x == 0 = 0
  | x == 1 = 1
  | otherwise = fibonacci (x-1) + fibonacci (x-2)

-- parteEntera function
parteEntera :: Float -> Int
parteEntera x
  | x <= 0 = undefined
  | x < 1 = 0
  | otherwise = 1 + parteEntera (x-1)

-- esDivisible function
esDivisible :: Int -> Int -> Bool
esDivisible x y
  | x < y = False
  | x == y = True
  | otherwise = esDivisible (x - y) y

sumaImparesAux :: Int -> Int
sumaImparesAux x | x==1 =1
                 | otherwise = 2 + sumaImparesAux (x-1) 

sumaImpares :: Int -> Int
sumaImpares x | x==1 =1
              | otherwise = sumaImparesAux x + sumaImpares (x-1) 

medioFact :: Integer ->Integer
medioFact 0 = 1
medioFact 1 = 1
medioFact x=  x* medioFact (x-2)

todosDigitosIguales :: Integer ->Bool
todosDigitosIguales 