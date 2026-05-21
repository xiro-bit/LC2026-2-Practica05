module Practica05 where

import Terminos



--Funcion auxiliar que levanta una bandera si hay sustitucion por hacer e indica la sustitucion
haySustitucion :: Term -> Subst -> (Bool, Term)
haySustitucion _ [] = (False, Var "foo")
haySustitucion (Var x) ((y, term):resto) = if x == y
                                        then (True,term)
                                        else haySustitucion (Var x) resto
haySustitucion _ _ = (False, Var "foo")


--Aplicar una sustitucion a un termino
apsubT :: Term -> Subst -> Term
apsubT (Var x) sigma = if bandera
                        then term
                        else (Var x)
                where
                    indicador = haySustitucion (Var x) sigma
                    bandera = fst indicador
                    term = snd indicador
apsubT (Fun f args) sigma = Fun f (aplicarLista args sigma)

--Funcion auxiliar para aplicar la sustitucion a una lista de terminos
aplicarLista :: [Term] -> Subst -> [Term]
aplicarLista [] _         = []
aplicarLista (t:ts) sigma = apsubT t sigma : aplicarLista ts sigma

--Funcion que elimina los pares que son de la forma x=x
simpSus :: Subst -> Subst
simpSus []             = []
simpSus ((x, term):resto)
    | Var x == term = simpSus resto
    | otherwise     = (x, term) : simpSus resto

--Funcion que calcula la composicion de dos sustituciones
compSus :: Subst -> Subst -> Subst
compSus sigma tau = simpSus (parteIzq ++ parteDer)
    where
        parteIzq = [(x, apsubT t tau) | (x, t) <- sigma]
        parteDer = [(x, t) | (x, t) <- tau, noEsta x sigma]
        noEsta n s = fst (haySustitucion (Var n) s) == False

--Funcion que devuelve un umg de dos terminos, si es que lo hay
unifica :: Term -> Term -> [Subst]
unifica (Var x) (Var y)
    | x == y    = [[]] -- Ya son iguales, unificador vacío
    | otherwise = [[(x, Var y)]]
unifica (Var x) t
    | estaEn x t = [] -- Fallo si la variable ya esta en un termino
    | otherwise  = [[(x, t)]]
unifica t (Var x)
    | estaEn x t = [] -- Fallo si la variable ya esta en un termino
    | otherwise  = [[(x, t)]]
unifica (Fun f args1) (Fun g args2)
    | f == g && length args1 == length args2 = unificaListas args1 args2
    | otherwise = [] -- Símbolos distintos o aridad distinta


--Funcion que devuelve un unificador de dos términos funcionales, si es que lo hay
unificaListas :: [Term] -> [Term] -> [Subst]
unificaListas [] [] = [[]]
unificaListas (t1:ts1) (t2:ts2) =
    case unifica t1 t2 of
        [] -> [] -- No unifican los primeros elementos
        [sigma] -> case unificaListas (aplicarLista ts1 sigma) (aplicarLista ts2 sigma) of
                        [] -> [] -- No unifican los restos tras aplicar la primera sustitución
                        [tau] -> [compSus sigma tau]
unificaListas _ _ = []

--Funcion que devuelve un umg de una lista de termino, si es que lo hay
unificaConj :: [Term] -> [Subst]
unificaConj [] = [[]]
unificaConj [_] = [[]]
unificaConj (t1:t2:ts) =
    case unifica t1 t2 of
        [] -> [] -- El primer par no unifica
        [sigma] -> case unificaConj (aplicarLista (t2:ts) sigma) of
                        [] -> []
                        [tau] -> [compSus sigma tau]

-- Funcion auxiliar que verifica si una variable aparece dentro de un termino
estaEn :: Nombre -> Term -> Bool
estaEn x (Var y)      = x == y
estaEn x (Fun _ args) = any (estaEn x) args