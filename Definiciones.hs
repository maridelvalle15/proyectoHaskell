module Definiciones where 
import Abecedario 

-- *** TERMINOS VALIDOS ***
true :: Term
true = Truee
false :: Term
false = Falsee

-- *** OPERADORES ***
neg :: Term -> Term
neg x = Neg x
infixl 8 \/
(\/) :: Term -> Term -> Term
x \/ y = Or x y
infixl 7 /\
(/\) :: Term -> Term -> Term
x /\ y = And x y
infixr 6 ==>
(==>) :: Term -> Term -> Term
x ==> y = Imp x y
infixl 5 <==>
(<==>) :: Term -> Term -> Term
x <==> y =  Equiv x y
infixl 4 !<==>
(!<==>) :: Term -> Term -> Term
x !<==> y =  Inequiv x y
infix 3 ===
(===) :: Term -> Term -> Equation
x === y = Igual x y
infix 1 =:
(=:) :: Term -> Term -> Sust     
x =: y = Sustit x y 

-- *** SUSTITUCION ***
sust :: Term -> (Sust) -> Term
sust Truee (Sustit t2 t3) = Truee
sust Falsee (Sustit t2 t3) = Falsee
sust (Var i) (Sustit (Var j) (Var k)) = if i==k then (Var j) else (Var i)
sust (Var i) (Sustit t1 (Var j)) = if i==j then t1 else (Var i)
-- sustitucion para \/
sust (Or t1 t2) (Sustit (Var i) (Var j)) = Or (sust t1 ((Var i)=:(Var j))) (sust t2 ((Var i)=:(Var j)))
sust (Or t1 t2) (Sustit t3 (Var j)) = Or (sust t1 (t3=:(Var j))) (sust t2 (t3=:(Var j)))
--sustitucion para /\
sust (And t1 t2) (Sustit (Var i) (Var j)) = And (sust t1 ((Var i)=:(Var j))) (sust t2 ((Var i)=:(Var j)))
sust (And t1 t2) (Sustit t3 (Var j)) = And (sust t1 (t3=:(Var j))) (sust t2 (t3=:(Var j)))
--sustitucion para ==>
sust (Imp t1 t2) (Sustit (Var i) (Var j)) = Imp (sust t1 ((Var i)=:(Var j))) (sust t2 ((Var i)=:(Var j)))
sust (Imp t1 t2) (Sustit t3 (Var j)) = Imp (sust t1 (t3=:(Var j))) (sust t2 (t3=:(Var j)))
--sustitucion para <==>
sust (Equiv t1 t2) (Sustit (Var i) (Var j)) = Equiv (sust t1 ((Var i)=:(Var j))) (sust t2 ((Var i)=:(Var j)))
sust (Equiv t1 t2) (Sustit t3 (Var j)) = Equiv (sust t1 (t3=:(Var j))) (sust t2 (t3=:(Var j)))
--sustitucion para !<==>
sust (Inequiv t1 t2) (Sustit (Var i) (Var j)) = Inequiv (sust t1 ((Var i)=:(Var j))) (sust t2 ((Var i)=:(Var j)))
sust (Inequiv t1 t2) (Sustit t3 (Var j)) = Inequiv (sust t1 (t3=:(Var j))) (sust t2 (t3=:(Var j)))

-- *** SHOW TERMS *** --
showTerm :: Term -> String
showTerm(Var i) = [i]
--Mostrar Neg
showTerm (Neg (Var i)) = "¬" ++ showTerm(Var i)
showTerm (Neg t) = "¬" ++ showTerm(t)
--Mostrar \/
showTerm(Or (Var i)(Var j)) = showTerm(Var i) ++ " \\/ " ++ showTerm(Var j)
showTerm(Or (Var i) t) = showTerm(Var i) ++ " \\/ (" ++ showTerm(t) ++ ")"
showTerm(Or t (Var i)) = "(" ++ showTerm(t) ++ ")" ++ " \\/ " ++ showTerm(Var i) 
showTerm (Or (Neg t1) t2) = "¬( "++ showTerm t1 ++ ") \\/ (" ++ showTerm t2 ++ ")" 
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
--Mostrar true y false
showTerm(Truee) = "true"
showTerm(Falsee) = "false"

-- *** SHOW SUST *** --
showSust :: Sust -> String
--Mostrar sustitucion
showSust(Sustit t1 t2) = showTerm t1 ++ " =: " ++ showTerm t2

-- *** INSTANCE ***
instance Show Term where show = showTerm
instance Show Sust where show = showSust