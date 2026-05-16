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
unifica = undefined


--Funcion que devuelve un unificador de dos términos funcionales, si es que lo hay
unificaListas :: [Term] -> [Term] -> [Subst]
unificaListas = undefined

--Funcion que devuelve un umg de una lista de termino, si es que lo hay
unificaConj :: [Term] -> [Subst]
unificaConj = undefined

