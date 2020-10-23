module Untyped where

import           Control.Monad
import           Data.List
import           Data.Maybe

import           Common

----------------------------------------------
-- Seccón 2  
-- Ejercicio 2: Conversión a términos localmente sin nombres
----------------------------------------------

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


conversion :: LamTerm -> Term
conversion lt = conversion' [] lt
                    where
                        conversion' lig (LVar s) =  case elemIndex s lig of
                                                        Just i  -> Bound i
                                                        _ -> Free (Global s)
                        conversion' lig (App lt1 lt2) = (conversion' lig lt1) :@: (conversion' lig lt2)
                        conversion' lig (Abs l lt) = Lam (conversion' (l:lig)  lt)

{-
Nuestra humilde implementación que no usamos por motivos de eficiencia
encontrarPosicion [] s i = Nothing
encontrarPosicion (x:xs) s i = if x==s then Just i else encontrarPosicion xs s (i+1) 
-}


-------------------------------
-- Sección 3
-------------------------------

{- 

data Value=
        VLam (Value → Value)
        |VNeutral Neutral
        
data Neutral=
        NFree Name
        |NApp Neutral Value

data Name = Global String | Quote Int

(VNeutral NFree "z") valores 
(VNeutral (NApp (NFree "z") (VLam pow²)))

 (z pow²) val = z (pow² val) 
(VNeutral (NApp (NFree "z") (VLam pow²))) val -> 

type NameEnv v = [(Name, v)]

data Term = Bound Int
        | Free Name
        | Term :@: Term
        | Lam Term

λx.t1 → λx.t′1 
funcion :: Int -> Int
funcion = \x -> x^2

funcion(3) = 9
 -}

vapp :: Value -> Value -> Value
vapp (VLam f) val = f val
vapp (VNeutral n) val = VNeutral (NApp n val) 

eval :: NameEnv Value -> Term -> Value
eval e t = eval' t (e, [])

eval' :: Term -> (NameEnv Value, [Value]) -> Value
eval' (Bound ii) (_, lEnv) = lEnv !! ii 
eval' (Free var) (entorno, lEnv)  = buscarVar entorno var
                where
                buscarVar [] var = VNeutral (NFree var)
                buscarVar ((nombre, val):xs) var = if nombre == var then val
                                                                    else buscarVar xs var
eval' (t1 :@: t2) (entorno, lEnv) = vapp (eval' t1 (entorno,lEnv)) (eval' t2 (entorno, lEnv))
eval' (Lam t) (entorno, lEnv) = VLam (\valor -> eval' t (entorno, valor:lEnv))

-- λx. t1 t2 ->β t1 [t2/x] 

--------------------------------
-- Sección 4 - Mostrando Valores
--------------------------------

{- 
quote :: Value -> Term
quote = quote_aux 0


(Vlam f) ... Lam ( quote_aux (n+1) (f (VNeutral (NFree(Quote n)))))

(VNeutral (NFree (Quote k)) = Bound (n-k-1))
 -}

quote :: Value -> Term
quote v = quote' v 0
        where
                quote' (VLam f) n = Lam (quote' (f (VNeutral (NFree (Quote n)))) (n+1))
                quote' (VNeutral (NFree (Quote k))) n  = Bound (n - k - 1)
                quote' (VNeutral (NFree (Global s))) n = Free (Global s)
                quote' (VNeutral (NApp neu v)) n = (quote' (VNeutral neu) n) :@: quote' v n
