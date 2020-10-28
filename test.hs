
aplicarNVeces:: (Int -> Int) -> Int -> Int -> Int 
aplicarNVeces f x 0 = x
aplicarNVeces f x n = aplicarNVeces f (f x) (n-1)

map':: (Int -> Int) -> [Int] -> Int -> [Int]
map' f [] n = []
map' f (x:xs) n = (aplicarNVeces f x n) : map' f xs (n+1)   

mapnHask:: (Int -> Int) -> [Int] -> [Int]
mapnHask f lista = map' f lista 0

-- λx y. z x --> λx (λy (z x)) === λλFREE1 

-- M = λy. x z
-- λx.M
-- Lam (Lam (Free (Global "z") :@: Bound 1))

{- 
data LamTerm = LVar String
            | App LamTerm LamTerm
            | Abs String LamTerm
 
data Term = Bound Int
        | Free Name
        | Term :@: Term
        | Lam Term
 
data Name = Global String | Quote Int

λx y. z x --> Abs "x" (Abs "y" (App (LVar "z") (LVar "x")))
Lam (Lam (Free (Global "z") :@: Bound 1))

EL NUESTRO BUENARDO
Lam (Lam (Free (Global "z") :@: Bound 1))
-}