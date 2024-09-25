module SolucionT2 where
  
relacionesValidas :: [(String, String)] -> Bool
relacionesValidas [] = False -- esta mal. lo tenes que corregir


personas :: [(String, String)] -> [String]
personas [] = ["nadie"] -- esta mal. lo tenes que corregir


amigosDe :: String -> [(String, String)] -> [String]
amigosDe "nadie" [] = ["nadie"] -- esta mal. lo tenes que corregir

personaConMasAmigos :: [(String, String)] -> String
personaConMasAmigos [] = "yo" -- esta mal. lo tenes que corregir




relacionesValidas :: [(String, String)] -> Bool
relacionesValidas [] = True
relacionesValidas ((x,y):xs) = tieneTuplasRepetidas ((x,y):xs) && ambosComponentesIguales ((x,y):xs)

tieneTuplasRepetidas :: [(String, String)] -> Bool
tieneTuplasRepetidas [] = True
tieneTuplasRepetidas ((x,y):(x1,y2):xs) | (x==x1) && (y==y2) = False
                                        | otherwise= tieneTuplasRepetidas ((x1,y2):xs) quitar ()

ambosComponentesIguales :: [(String, String)] -> Bool
ambosComponentesIguales [] = True
ambosComponentesIguales ((x,y):xs) | x==y = False
                                   | otherwise= ambosComponentesIguales xs

quitar ::  String -> [String] -> [String]
quitar x (y:ys) | x == y = ys
                | otherwise = y : quitar x ys



problema personas (relaciones: seq⟨String x String⟩) : seq⟨String⟩ {
  requiere: {relacionesValidas(relaciones)}
  asegura: {res no tiene elementos repetidos}
  asegura: {res tiene exactamente los elementos que figuran en alguna tupla de relaciones, en cualquiera de sus posiciones}
}

personas :: [(String, String)] -> [String]
personas [] = [] 
personas ((x,y):xs) | !relacionesValidas ((x,y):xs) && = []
                    | otherwise =  listaPersonas (x:y:xs)
listaPersonas:: [String]->[String]
listaPersonas [] = []
listaPersonas (x:y:xs)  | x==y = x: listaPersonas xs
                        | otherwise= x:listaPersonas (y:xs) 

problema amigosDe (persona: String, relaciones: seq⟨String x String⟩) : seq⟨String⟩ {
  requiere: {relacionesValidas(relaciones)}
  asegura: {res tiene exactamente los elementos que figuran en las tuplas de relaciones en las que una de sus componentes es persona}
} 

amigosDe :: String -> [(String, String)] -> [String]
amigosDe _ [] = [] 
amigosDe y ((x,x1):xs) | (y==x || y==x1) = amigosDe y:xs

problema personaConMasAmigos (relaciones: seq⟨String x String⟩) : String {
  requiere: {relaciones no vacía}
  requiere: {relacionesValidas(relaciones)}
  asegura: {res es el Strings que aparece más veces en las tuplas de relaciones (o alguno de ellos si hay empate)}
}