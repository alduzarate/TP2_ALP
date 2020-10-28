module Untyped where

import           Control.Monad
import           Data.List
import           Data.Maybe

import           Common

----------------------------------------------
-- Sección 2  
-- Ejercicio 2: Conversión a términos localmente sin nombres
----------------------------------------------

conversion :: LamTerm -> Term
conversion lt = conversion' [] lt
                    where
                        conversion' lig (LVar s) =  case elemIndex s lig of
                                                        Just i  -> Bound i
                                                        _ -> Free (Global s)
                        conversion' lig (App lt1 lt2) = (conversion' lig lt1) :@: (conversion' lig lt2)
                        conversion' lig (Abs l lt) = Lam (conversion' (l:lig)  lt)

{-
Una humilde implementación que al final no usamos por motivos de eficiencia
encontrarPosicion [] s i = Nothing
encontrarPosicion (x:xs) s i = if x==s then Just i else encontrarPosicion xs s (i+1) 
-}


-------------------------------
-- Sección 3
-------------------------------

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

--------------------------------
-- Sección 4 - Mostrando Valores
--------------------------------

quote :: Value -> Term
quote v = quote' v 0
        where
                quote' (VLam f) n = Lam (quote' (f (VNeutral (NFree (Quote n)))) (n+1))
                quote' (VNeutral (NFree (Quote k))) n  = Bound (n - k - 1)
                quote' (VNeutral (NFree (Global s))) n = Free (Global s)
                quote' (VNeutral (NApp neu v)) n = (quote' (VNeutral neu) n) :@: quote' v n
