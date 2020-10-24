
aplicarNVeces:: (Int -> Int) -> Int -> Int -> Int 
aplicarNVeces f x 0 = x
aplicarNVeces f x n = aplicarNVeces f (f x) (n-1)

map':: (Int -> Int) -> [Int] -> Int -> [Int]
map' f [] n = []
map' f (x:xs) n = (aplicarNVeces f x n) : map' f xs (n+1)   

mapnHask:: (Int -> Int) -> [Int] -> [Int]
mapnHask f lista = map' f lista 0