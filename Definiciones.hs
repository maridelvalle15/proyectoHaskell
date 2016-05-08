module Definiciones where 
import Abecedario 

--terminos validos
true :: Term
true = Truee
false :: Term
false = Falsee

--operadores
neg :: Term -> Term
neg x = Neg x
infix 8 \/
(\/) :: Term -> Term -> Term
x \/ y = Or x y
infix 7 /\
(/\) :: Term -> Term -> Term
x /\ y = And x y
infix 6 ==>
(==>) :: Term -> Term -> Term
x ==> y = Imp x y
infix 5 <==>
(<==>) :: Term -> Term -> Term
x <==> y =  Equiv x y
infix 4 !<==>
(!<==>) :: Term -> Term -> Term
x !<==> y =  Inequiv x y
infix 3 ===
(===) :: Term -> Term -> Equation
x === y = Igual x y
infix 1 =:
(=:) :: Term -> Term -> Sust     
x =: y = sust x y 
{-
--sustitucion
--sust :: Term -> Sust -> Term
--sust term sust = if t1==t2 then term2 else (Var s1)
--sust (Sum term1 term2) (var,term) = Sum (sust term1 (var,term)) (sust term2 (var,term))
-}
showTerm :: Term -> String
showTerm(Var i) = [i]
--Mostrar \/
showTerm(Or (Var i)(Var j)) = showTerm(Var i) ++ " \\/ " ++ showTerm(Var j)
showTerm(Or (Var i) t) = showTerm(Var i) ++ " \\/ (" ++ showTerm(t) ++ ")"
showTerm(Or t (Var i)) = "(" ++ showTerm(t) ++ ")" ++ " \\/ " ++ showTerm(Var i) 
showTerm (Or t1 t2) = "(" ++ showTerm t1 ++ ") \\/ (" ++ showTerm t2 ++ ")"
 --Mostrar /\
showTerm(And (Var i)(Var j)) = showTerm(Var i) ++ " /\\ " ++ showTerm(Var j)
showTerm(And (Var i) t) = showTerm(Var i) ++ " /\\ (" ++ showTerm(t) ++ ")"
showTerm(And t (Var i)) = "(" ++ showTerm(t) ++ ")" ++ " /\\ " ++ showTerm(Var i) 
showTerm (And t1 t2) = "(" ++ showTerm t1 ++ ") /\\ (" ++ showTerm t2 ++ ")"
 --Mostrar ==>
showTerm(Imp (Var i)(Var j)) = showTerm(Var i) ++ " ==> " ++ showTerm(Var j)
showTerm(Imp (Var i) t) = showTerm(Var i) ++ " ==> (" ++ showTerm(t) ++ ")"
showTerm(Imp t (Var i)) = "(" ++ showTerm(t) ++ ")" ++ " ==> " ++ showTerm(Var i) 
showTerm (Imp t1 t2) = "(" ++ showTerm t1 ++ ") ==> (" ++ showTerm t2 ++ ")"
 --Mostrar <==>
showTerm(Equiv (Var i)(Var j)) = showTerm(Var i) ++ " <==> " ++ showTerm(Var j)
showTerm(Equiv (Var i) t) = showTerm(Var i) ++ " <==> (" ++ showTerm(t) ++ ")"
showTerm(Equiv t (Var i)) = "(" ++ showTerm(t) ++ ")" ++ " <==> " ++ showTerm(Var i) 
showTerm (Equiv t1 t2) = "(" ++ showTerm t1 ++ ") <==> (" ++ showTerm t2 ++ ")"
 --Mostrar !<==>
showTerm(Inequiv (Var i)(Var j)) = showTerm(Var i) ++ " !<==> " ++ showTerm(Var j)
showTerm(Inequiv (Var i) t) = showTerm(Var i) ++ " !<==> (" ++ showTerm(t) ++ ")"
showTerm(Inequiv t (Var i)) = "(" ++ showTerm(t) ++ ")" ++ " !<==> " ++ showTerm(Var i) 
showTerm (Inequiv t1 t2) = "(" ++ showTerm t1 ++ ") !<==> (" ++ showTerm t2 ++ ")"
--Mostrar Neg
showTerm (Neg (Var i)) = "¬" ++ showTerm(Var i)
showTerm (Neg t) = "¬(" ++ showTerm(t) ++")"
--Mostra true y false
showTerm(Truee) = "true"
showTerm(Falsee) = "false"

instance Show Term where show = showTerm