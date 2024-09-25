type Posicion = [Int]
type Jugada = (Int,Int)
type Set a = [a]

-- Ejercicio 1:

-- busca la lista indicada y resta la cantidad correspondiente.
-- En caso de quitar todos los palitos de una lista, la elimina.

jugar :: Posicion -> Jugada -> Posicion
jugar [] _ = [] 
jugar p j  = buscaListaYResta p j   

buscaListaYResta :: Set Int -> (Int,Int) -> Set Int
buscaListaYResta (x:xs) (n,m) | n == 1 && (x - m) /= 0  = (x - m):xs 
                              | n == 1 && (x - m) == 0  = xs
                              | otherwise               = x:(buscaListaYResta xs (n-1,m)) 

-- Ejercicio 2:

-- genera todos los productos cartesianos del número de lista contra 
-- todos los números del 1 hasta el entero que haya en esa lista.

generarTuplas :: Int -> Set Int -> Set (Int,Int)
generarTuplas x []     = [] 
generarTuplas x (y:ys) = (x,y):(generarTuplas x ys) 

productoCartesiano :: Set Int -> Set Int -> Set (Int,Int)
productoCartesiano [] _      = []
productoCartesiano (x:xs) ys = (generarTuplas x ys) ++ (productoCartesiano xs ys)

posiblesJugadas :: Posicion -> [Jugada]
posiblesJugadas [] = [] 
posiblesJugadas p  = desde1 p 

generarProductos :: Int -> Set Int -> Set (Int,Int) 
generarProductos _ []     = []
generarProductos n (x:xs) = productoCartesiano [n] [1..x] ++ generarProductos (n + 1) xs 
--
desde1 :: Set Int -> Set (Int,Int)
desde1 (x:xs) = generarProductos 1 (x:xs)  

-- Ejercicio 3:


{-

esPosicionGanadora :: Posicion -> Bool
esPosicionGanadora p = not (esSumaNim p)  

esSumaNim :: Set Int -> Bool
esSumaNim p = esPar (sumaCabezas (listaCompleta (p)) && esPar (sumaCabezas (eliminarPrimerElementoDeCadaLista (listaCompleta p)))  

esSumaNim :: Set Int -> Bool
esSumaNim p = esPar (sumaCabezas (listaCompleta (p)) && esSumaNim 





-}

unoAUno :: Set [Int] -> Int 
unoAUno [] = 0
unoAUno (x:xs) = sumaCabezas (x:xs) + unoAUno (xs)  


eliminarPrimerElementoDeCadaLista :: Set [Int] -> Set [Int]
eliminarPrimerElementoDeCadaLista [] = [] 
eliminarPrimerElementoDeCadaLista (x:xs) = concatenar (tail [x]) (eliminarPrimerElementoDeCadaLista (xs)) 


sumaCabezas :: Set [Int] -> Int 
sumaCabezas [] = 0
sumaCabezas x = head (head x) + sumaCabezas (tail x) 


----------------------------------------------------------------------------------------------------------------------------

listaCompleta :: Set Int -> Set [Int]
listaCompleta x = completaBinarios (listaBinarios x) x  

completaBinarios :: Set [Int] -> Set Int -> Set [Int]
completaBinarios [] _ = []
completaBinarios x p | longitud (head x) == longitudMaxima p = concatenar [head x] (completaBinarios (tail x) p)  
                     | otherwise = completaBinarios ((concatenar [0:(head x)] (tail x))) p

concatenar :: Set [Int] -> Set [Int] -> Set [Int]
concatenar xs ys = agregarAlFinalTodos xs ys 

agregarAlFinalTodos :: Set [Int] -> Set [Int] -> Set [Int]
agregarAlFinalTodos xs [] = xs 
agregarAlFinalTodos xs (y:ys) = agregarAlFinalTodos (agregarAlFinal2 y xs) ys

agregarAlFinal2 :: [Int] -> Set [Int] -> Set [Int] 
agregarAlFinal2 x [] = [x]
agregarAlFinal2 x (y:ys) = y:(agregarAlFinal2 x ys) 

listaBinarios :: Set Int -> Set [Int]
listaBinarios [] = []
listaBinarios (x:xs) = concatenar [convertirABinario x] (listaBinarios xs)  

longitudMaxima :: Set Int -> Int 
longitudMaxima p = longitud (convertirABinario (maximo p)) 

maximo :: [Int] -> Int 
maximo [] = 0
maximo (x:xs) | x >= maximo xs = x 
              | otherwise = maximo xs 

vacio :: Set a
vacio = []

agregar :: Eq a => a -> Set a -> Set a
agregar n c | n `elem` c = c
            | otherwise  = n:c

union :: Eq a => Set a -> Set a -> Set a
union [] ys = ys
union (x:xs) ys = union xs (agregar x ys)

longitud :: Set a -> Int
longitud [] = 0
longitud (_:xs) = 1 + longitud xs 

esPar :: Int -> Bool
esPar x = mod x 2 == 0

convertirABinarioAlReves :: Int -> Set Int
convertirABinarioAlReves 0 = [0]
convertirABinarioAlReves 1 = [1]
convertirABinarioAlReves x | div x 2 == 1 = final x   
                           | otherwise    = [mod x 2] ++ convertirABinarioAlReves(div x 2) 

final :: Int -> Set Int 
final x | mod x 2 == 1 = [1,1]
        | mod x 2 == 0 = [0,1] 

reverso :: [Int] -> [Int]
reverso [] = []
reverso (x:xs) = agregarAlFinal x (reverso xs) 

agregarAlFinal :: Int -> [Int] -> [Int] 
agregarAlFinal x [] = [x]
agregarAlFinal x (y:ys) = y:(agregarAlFinal x ys) 

convertirABinario :: Int -> Set Int
convertirABinario x = reverso (convertirABinarioAlReves x) 

