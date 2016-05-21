{-# LANGUAGE FlexibleInstances#-}
module Definiciones where 
import Theorems
import Term


-- *** SHOW TERMS ***
showTerm :: Term -> String
showTerm(Var i) = [i]

--Mostrar ¬
showTerm (Neg Truee) = "¬true"
showTerm (Neg Falsee) = "¬false"
showTerm (Neg (Var i)) = "¬" ++ showTerm(Var i)
--Mostrar ¬ \/
showTerm(Or (Neg t1) (Neg t2)) = showTerm(Neg t1) ++ " \\/ " ++ showTerm(Neg t2)
showTerm(Or (Var i) (Neg t)) = showTerm(Var i) ++ " \\/ " ++ showTerm(Neg t)
showTerm(Or (Neg t) (Var i)) = showTerm(Neg t) ++ " \\/ " ++ showTerm(Var i)
showTerm(Or (Neg t) (t1)) = showTerm(Neg t) ++ " \\/ " ++ showTerm(t1)
showTerm(Or (t1) (Neg t)) = showTerm(t1) ++ " \\/ " ++ showTerm(Neg t)
showTerm (Neg (Or t1 t2 )) = "¬(" ++ showTerm (Or t1 t2) ++ ")"
--Mostrar ¬ /\
showTerm (And (Neg t1) (Neg t2)) =   showTerm (Neg t1) ++ "/\\" ++ showTerm(Neg t2) 
showTerm (Neg (And t1 t2 )) = "¬(" ++ showTerm (And t1 t2) ++ ")"
showTerm(And (Var i) (Neg t)) = showTerm(Var i) ++ " /\\ " ++ showTerm(Neg t)
showTerm(And (Neg i)(Var j)) = showTerm(Neg i) ++ " /\\ " ++ showTerm(Var j)
showTerm(And (Neg t) t1) = showTerm(Neg t) ++ " /\\ "  ++ showTerm(t1) 
showTerm(And t1 (Neg t)) = showTerm(t1) ++  " /\\ " ++ showTerm(Neg t) 
 --Mostrar ¬ ==>
showTerm(Imp (Var i) (Neg t)) = showTerm(Var i) ++ " ==> " ++ showTerm(Neg t)
showTerm(Imp (Neg i)(Var j)) = showTerm(Neg i) ++ " ==> " ++ showTerm(Var j)
showTerm(Imp (Neg t1)(Neg t2)) = showTerm(Neg t1) ++ " ==> " ++ showTerm(Neg t2)
showTerm(Imp (Neg t) (Equiv t1 t2)) = showTerm(Neg t) ++ " ==> (" ++ showTerm(Equiv t1 t2) ++ ")"
showTerm(Imp (Equiv t1 t2) (Neg t)) = "(" ++ showTerm(Equiv t1 t2) ++ ") ==> " ++ showTerm(Neg t)
showTerm(Imp (Neg t) t1) = showTerm(Neg t) ++ " ==> " ++ showTerm(t1)
showTerm(Imp t1 (Neg t)) = showTerm(t1) ++ " ==> " ++ showTerm(Neg t) 
showTerm (Neg (Imp t1 t2)) = "¬(" ++ showTerm (Imp t1 t2) ++ ")"

 --Mostrar ¬ <==>
showTerm(Equiv (Var i) (Neg t)) = showTerm(Var i) ++ " <==> " ++ showTerm(Neg t)
showTerm(Equiv (Neg i)(Var j)) = showTerm(Neg i) ++ " <==> " ++ showTerm(Var j)
showTerm(Equiv (Neg t1)(Neg t2)) = showTerm(Neg t1) ++ " <==> " ++ showTerm(Neg t2)
showTerm(Equiv (Neg t) (Inequiv t1 t2) ) = showTerm(Neg t) ++ " <==> (" ++ showTerm(Inequiv t1 t2) ++ ")" 
showTerm(Equiv (Neg t) (Equiv t1 t2) ) = showTerm(Neg t) ++ " <==> (" ++ showTerm(Equiv t1 t2) ++ ")" 
showTerm(Equiv (Neg t) t1) = showTerm(Neg t) ++ " <==> " ++ showTerm(t1)
showTerm(Equiv (Inequiv t1 t2) (Neg t)) = "(" ++ showTerm(Inequiv t1 t2) ++ ") <==> " ++ showTerm(Neg t) 
showTerm(Equiv (Equiv t1 t2) (Neg t)) = "(" ++ showTerm(Equiv t1 t2) ++ ") <==> " ++ showTerm(Neg t) 
showTerm(Equiv t1 (Neg t)) = showTerm(t1) ++ " <==> " ++ showTerm(Neg t) 
showTerm (Neg (Equiv t1 t2)) = "¬(" ++ showTerm (Equiv t1 t2) ++ ")"
 --Mostrar ¬ !<==> 
showTerm(Inequiv (Var i) (Neg t)) = showTerm(Var i) ++ " !<==> " ++ showTerm(Neg t)
showTerm(Inequiv (Neg i)(Var j)) = showTerm(Neg i) ++ " !<==> " ++ showTerm(Var j) 
showTerm(Inequiv (Neg t1)(Neg t2)) = showTerm(Neg t1) ++ " !<==> " ++ showTerm(Neg t2)
showTerm(Inequiv (Neg t) t1) = showTerm(Neg t) ++ " !<==> " ++ showTerm(t1)
showTerm(Inequiv t1 (Neg t)) = showTerm(t1) ++ " !<==> " ++ showTerm(Neg t) 
showTerm (Neg (Inequiv t1 t2)) = "¬(" ++ showTerm (Inequiv t1 t2) ++ ")"

-- Mostrar ¬ termino
showTerm (Neg t1) = "¬(" ++ showTerm t1 ++ ")"

--Mostrar \/
showTerm(Or (Falsee)(Falsee)) = showTerm(Falsee) ++ " \\/ " ++ showTerm(Falsee)
showTerm(Or (Falsee)(Truee)) = showTerm(Falsee) ++ " \\/ " ++ showTerm(Truee)
showTerm(Or (Truee)(Falsee)) = showTerm(Truee) ++ " \\/ " ++ showTerm(Falsee)
showTerm(Or (Truee)(Truee)) = showTerm(Truee) ++ " \\/ " ++ showTerm(Truee)
showTerm(Or (Truee)(Var i)) = showTerm(Truee) ++ " \\/ " ++ showTerm(Var i)
showTerm(Or (Var i)(Truee)) = showTerm(Var i) ++ " \\/ " ++ showTerm(Truee)
showTerm(Or (Truee) t) = showTerm(Truee) ++ " \\/ (" ++ showTerm(t) ++ ")"
showTerm(Or t (Truee)) = "(" ++ showTerm(t) ++ ")" ++ " \\/ " ++ showTerm(Truee)
showTerm(Or (Falsee)(Var i)) = showTerm(Falsee) ++ " \\/ " ++ showTerm(Var i)
showTerm(Or (Var i)(Falsee)) = showTerm(Var i) ++ " \\/ " ++ showTerm(Falsee)
showTerm(Or (Falsee) t) = showTerm(Falsee) ++ " \\/ (" ++ showTerm(t) ++ ")"
showTerm(Or t (Falsee)) = "(" ++ showTerm(t) ++ ")" ++ " \\/ " ++ showTerm(Falsee)
showTerm(Or (Var i)(Var j)) = showTerm(Var i) ++ " \\/ " ++ showTerm(Var j)
showTerm(Or (Var i) t) = showTerm(Var i) ++ " \\/ (" ++ showTerm(t) ++ ")"
showTerm(Or t (Var i)) = "(" ++ showTerm(t) ++ ")" ++ " \\/ " ++ showTerm(Var i)
showTerm (Or t1 t2) = "(" ++ showTerm t1 ++ ") \\/ (" ++ showTerm t2 ++ ")"

 --Mostrar /\
showTerm(And (Falsee)(Falsee)) = showTerm(Falsee) ++ " /\\ " ++ showTerm(Falsee)
showTerm(And (Falsee)(Truee)) = showTerm(Falsee) ++ " /\\ " ++ showTerm(Truee)
showTerm(And (Truee)(Falsee)) = showTerm(Truee) ++ " /\\ " ++ showTerm(Falsee)
showTerm(And (Truee)(Truee)) = showTerm(Truee) ++ " /\\ " ++ showTerm(Truee)
showTerm(And (Truee)(Var i)) = showTerm(Truee) ++ " /\\ " ++ showTerm(Var i)
showTerm(And (Var i)(Truee)) = showTerm(Var i) ++ " /\\ " ++ showTerm(Truee)
showTerm(And (Truee) t) = showTerm(Truee) ++ "/\\ (" ++ showTerm(t) ++ ")"
showTerm(And t (Truee)) = "(" ++ showTerm(t) ++ ")" ++ " /\\ " ++ showTerm(Truee)
showTerm(And (Falsee)(Var i)) = showTerm(Falsee) ++ " /\\ " ++ showTerm(Var i)
showTerm(And (Var i)(Falsee)) = showTerm(Var i) ++ " /\\ " ++ showTerm(Falsee)
showTerm(And (Falsee) t) = showTerm(Falsee) ++ " /\\ (" ++ showTerm(t) ++ ")"
showTerm(And t (Falsee)) = "(" ++ showTerm(t) ++ ")" ++ " /\\ " ++ showTerm(Falsee)
showTerm(And (Var i)(Var j)) = showTerm(Var i) ++ " /\\ " ++ showTerm(Var j)
showTerm(And (Var i) t) = showTerm(Var i) ++ " /\\ (" ++ showTerm(t) ++ ")"
showTerm(And t (Var i)) = "(" ++ showTerm(t) ++ ")" ++ "/\\ " ++ showTerm(Var i) 
showTerm (And t1 t2) = "(" ++ showTerm t1 ++ ") /\\ (" ++ showTerm t2 ++ ")"
 --Mostrar ==>
 -- True y False
showTerm(Imp (Falsee)(Falsee)) = showTerm(Falsee) ++ " ==> " ++ showTerm(Falsee)
showTerm(Imp (Falsee)(Truee)) = showTerm(Falsee) ++ " ==> " ++ showTerm(Truee)
showTerm(Imp (Truee)(Falsee)) = showTerm(Truee) ++ " ==> " ++ showTerm(Falsee)
showTerm(Imp (Truee)(Truee)) = showTerm(Truee) ++ " ==> " ++ showTerm(Truee)
showTerm(Imp (Truee)(Var i)) = showTerm(Truee) ++ " ==> " ++ showTerm(Var i)
showTerm(Imp (Var i)(Truee)) = showTerm(Var i) ++ " ==> " ++ showTerm(Truee)
showTerm(Imp (Truee) t) = showTerm(Truee) ++ "==> (" ++ showTerm(t) ++ ")"
showTerm(Imp t (Truee)) = "(" ++ showTerm(t) ++ ")" ++ " ==> " ++ showTerm(Truee)
showTerm(Imp (Falsee)(Var i)) = showTerm(Falsee) ++ " ==> " ++ showTerm(Var i)
showTerm(Imp (Var i)(Falsee)) = showTerm(Var i) ++ " ==> " ++ showTerm(Falsee)
showTerm(Imp (Falsee) t) = showTerm(Falsee) ++ " ==> (" ++ showTerm(t) ++ ")"
showTerm(Imp t (Falsee)) = "(" ++ showTerm(t) ++ ")" ++ " ==> " ++ showTerm(Falsee)
-- Variables y terminos
showTerm(Imp (Var i)(Var j)) = showTerm(Var i) ++ " ==> " ++ showTerm(Var j)

showTerm(Imp (Var i) (Equiv t1 t2)) = showTerm(Var i) ++ " ==> (" ++ showTerm(Equiv t1 t2) ++ ")"
showTerm(Imp (Equiv t1 t2) (Var i)) = "(" ++ showTerm(Equiv t1 t2) ++ ") ==> " ++ showTerm(Var i)
showTerm(Imp (Equiv t1 t2) (Inequiv t3 t4)) = "(" ++ showTerm(Equiv t1 t2) ++ ") ==> (" ++ showTerm(Inequiv t3 t4) ++ ")"
showTerm(Imp (Inequiv t1 t2) (Equiv t3 t4)) = "(" ++ showTerm(Inequiv t1 t2) ++ ") ==> (" ++ showTerm(Equiv t3 t4) ++ ")"
showTerm(Imp (Equiv t1 t2) (Equiv t3 t4)) = "(" ++ showTerm(Equiv t1 t2) ++ ") ==> (" ++ showTerm(Equiv t3 t4) ++ ")"
showTerm(Imp t (Equiv t1 t2)) = showTerm(t) ++ " ==> (" ++ showTerm(Equiv t1 t2) ++ ")"
showTerm(Imp (Equiv t1 t2) t) = "(" ++ showTerm(Equiv t1 t2) ++ ") ==> " ++ showTerm(t)
showTerm(Imp (Var i) (Inequiv t1 t2)) = showTerm(Var i) ++ " ==> (" ++ showTerm(Inequiv t1 t2) ++ ")"
showTerm(Imp (Inequiv t1 t2) (Var i)) = "(" ++ showTerm(Inequiv t1 t2) ++ ") ==> " ++ showTerm(Var i)
showTerm(Imp (Inequiv t1 t2) (Inequiv t3 t4)) = "(" ++ showTerm(Inequiv t1 t2) ++ ") ==> (" ++ showTerm(Inequiv t3 t4) ++ ")"
showTerm(Imp t (Inequiv t1 t2)) = showTerm(t) ++ " ==> (" ++ showTerm(Inequiv t1 t2) ++ ")"
showTerm(Imp (Inequiv t1 t2) t) = "(" ++ showTerm(Inequiv t1 t2) ++ ") ==> " ++ showTerm(t)
showTerm(Imp (Var i) t) = showTerm(Var i) ++ " ==> " ++ showTerm(t)
showTerm(Imp t (Var i)) = showTerm(t) ++ " ==> " ++ showTerm(Var i) 
showTerm (Imp t1 t2) = showTerm t1 ++ " ==> " ++ showTerm t2
 --Mostrar <==>
showTerm(Equiv (Falsee)(Falsee)) = showTerm(Falsee) ++ " <==> " ++ showTerm(Falsee)
showTerm(Equiv (Falsee)(Truee)) = showTerm(Falsee) ++ " <==> " ++ showTerm(Truee)
showTerm(Equiv (Truee)(Falsee)) = showTerm(Truee) ++ " <==> " ++ showTerm(Falsee)
showTerm(Equiv (Truee)(Truee)) = showTerm(Truee) ++ " <==> " ++ showTerm(Truee)
showTerm(Equiv (Truee)(Var i)) = showTerm(Truee) ++ " <==> " ++ showTerm(Var i)
showTerm(Equiv (Var i)(Truee)) = showTerm(Var i) ++ " <==> " ++ showTerm(Truee)
showTerm(Equiv (Truee) (Equiv t1 t2)) = showTerm(Truee) ++ " <==> (" ++ showTerm(Equiv t1 t2) ++ ")"
showTerm(Equiv (Truee) (Inequiv t1 t2)) = showTerm(Truee) ++ " <==> (" ++ showTerm(Inequiv t1 t2) ++ ")"
showTerm(Equiv (Truee) t) = showTerm(Truee) ++ " <==> " ++ showTerm(t)
showTerm(Equiv (Equiv t1 t2) (Truee)) = "(" ++ showTerm(Equiv t1 t2) ++ ")" ++ " <==> " ++ showTerm(Truee)
showTerm(Equiv (Inequiv t1 t2) (Truee)) = "(" ++ showTerm(Inequiv t1 t2) ++ ")" ++ " <==> " ++ showTerm(Truee)
showTerm(Equiv t (Truee)) = showTerm(t) ++ " <==> " ++ showTerm(Truee)
showTerm(Equiv (Falsee)(Var i)) = showTerm(Falsee) ++ " <==> " ++ showTerm(Var i)
showTerm(Equiv (Var i)(Falsee)) = showTerm(Var i) ++ " <==> " ++ showTerm(Falsee)
showTerm(Equiv (Falsee) (Equiv t1 t2)) = showTerm(Falsee) ++ " <==> (" ++ showTerm(Equiv t1 t2) ++ ")"
showTerm(Equiv (Falsee) (Inequiv t1 t2)) = showTerm(Falsee) ++ " <==> (" ++ showTerm(Inequiv t1 t2) ++ ")"
showTerm(Equiv (Falsee) t) = showTerm(Falsee) ++ " <==> " ++ showTerm(t)
showTerm(Equiv t (Falsee)) = "(" ++ showTerm(t) ++ ")" ++ " <==> " ++ showTerm(Falsee)
showTerm(Equiv (Equiv t1 t2) (Equiv t3 t4)) = "(" ++ showTerm (Equiv t1 t2) ++ ") <==> (" ++ showTerm (Equiv t3 t4) ++ ")"
showTerm(Equiv (Equiv t1 t2) (Var i)) = "(" ++ showTerm(Equiv t1 t2) ++ ") <==>" ++ showTerm(Var i)
showTerm(Equiv (Equiv t1 t2) t3) = "(" ++ showTerm (Equiv t1 t2) ++ ") <==> " ++ showTerm t3
showTerm(Equiv (Var i)(Var j)) = showTerm(Var i) ++ " <==> " ++ showTerm(Var j)
showTerm(Equiv (Var i) (Equiv t1 t2)) = showTerm(Var i) ++ " <==> (" ++ showTerm(Equiv t1 t2) ++ ")"
showTerm(Equiv (Var i) (Inequiv t1 t2)) = showTerm(Var i) ++ " <==> (" ++ showTerm(Inequiv t1 t2) ++ ")"
showTerm(Equiv (Inequiv t1 t2) (Var i)) = "(" ++ showTerm(Inequiv t1 t2) ++ ") <==> " ++ showTerm (Var i)
showTerm(Equiv (Var i) t) = showTerm(Var i) ++ " <==> " ++ showTerm(t)
showTerm(Equiv t (Var i)) = showTerm(t) ++ " <==> " ++ showTerm(Var i)
showTerm (Equiv t2 (Equiv t t1)) = showTerm (t2) ++ " <==> (" ++ showTerm (Equiv t t1) ++ ")"
showTerm (Equiv t1 t2) = showTerm t1 ++ " <==> " ++ showTerm t2

 --Mostrar !<==>
showTerm(Inequiv (Falsee)(Falsee)) = showTerm(Falsee) ++ " !<==> " ++ showTerm(Falsee)
showTerm(Inequiv (Falsee)(Truee)) = showTerm(Falsee) ++ " !<==> " ++ showTerm(Truee)
showTerm(Inequiv (Truee)(Falsee)) = showTerm(Truee) ++ " !<==> " ++ showTerm(Falsee)
showTerm(Inequiv (Truee)(Truee)) = showTerm(Truee) ++ " !<==> " ++ showTerm(Truee)
showTerm(Inequiv (Truee)(Var i)) = showTerm(Truee) ++ " !<==> " ++ showTerm(Var i)
showTerm(Inequiv (Var i)(Truee)) = showTerm(Var i) ++ " !<==> " ++ showTerm(Truee)
showTerm(Inequiv (Truee) (Equiv t1 t2)) = showTerm(Truee) ++ " !<==> (" ++ showTerm(Equiv t1 t2) ++ ")"
showTerm(Inequiv (Truee) (Inequiv t1 t2)) = showTerm(Truee) ++ " !<==> (" ++ showTerm(Inequiv t1 t2) ++ ")"
showTerm(Inequiv (Truee) t) = showTerm(Truee) ++ " !<==> " ++ showTerm(t)
showTerm(Inequiv (Equiv t1 t2) (Truee)) = "(" ++ showTerm(Equiv t1 t2) ++ ")" ++ " !<==> " ++ showTerm(Truee)
showTerm(Inequiv (Inequiv t1 t2) (Truee)) = "(" ++ showTerm(Inequiv t1 t2) ++ ")" ++ " !<==> " ++ showTerm(Truee)
showTerm(Inequiv t (Truee)) = showTerm(t) ++ " !<==> " ++ showTerm(Truee)
showTerm(Inequiv (Falsee)(Var i)) = showTerm(Falsee) ++ " !<==> " ++ showTerm(Var i)
showTerm(Inequiv (Var i)(Falsee)) = showTerm(Var i) ++ " !<==> " ++ showTerm(Falsee)
showTerm(Inequiv (Falsee) (Equiv t1 t2)) = showTerm(Falsee) ++ " !<==> (" ++ showTerm(Equiv t1 t2) ++ ")"
showTerm(Inequiv (Falsee) (Inequiv t1 t2)) = showTerm(Falsee) ++ " !<==> (" ++ showTerm(Inequiv t1 t2) ++ ")"
showTerm(Inequiv (Falsee) t) = showTerm(Falsee) ++ " !<==> " ++ showTerm(t)
showTerm(Inequiv t (Falsee)) = "(" ++ showTerm(t) ++ ")" ++ " !<==> " ++ showTerm(Falsee)
showTerm(Inequiv (Var i)(Var j)) = showTerm(Var i) ++ " !<==> " ++ showTerm(Var j)
showTerm(Inequiv (Var i) (Equiv t1 t2)) = showTerm(Var i) ++ " !<==> (" ++ showTerm(Equiv t1 t2) ++ ")"
showTerm(Inequiv (Var i) (Inequiv t1 t2)) = showTerm(Var i) ++ " !<==> (" ++ showTerm(Inequiv t1 t2) ++ ")"
showTerm(Inequiv (Var i) t) = showTerm(Var i) ++ " !<==> " ++ showTerm(t)
showTerm(Inequiv (Equiv t1 t2) (Var i)) = "(" ++ showTerm(Equiv t1 t2) ++ ") !<==> " ++ showTerm(Var i) 
showTerm(Inequiv (Inequiv t1 t2) (Var i)) = "(" ++ showTerm(Inequiv t1 t2) ++ ") !<==> " ++ showTerm(Var i) 
showTerm(Inequiv t (Var i)) = showTerm(t) ++ " !<==> " ++ showTerm(Var i) 
showTerm (Inequiv t1 t2) = showTerm t1 ++ " !<==> " ++ showTerm t2 
--Mostrar true y false
showTerm(Truee) = "true"
showTerm(Falsee) = "false"


-- *** SHOW EQUATION *** --
showEquation :: Equation -> String
--Mostrar sustitucion
showEquation(Igual t1 t2) = showTerm t1 ++ " === " ++ showTerm t2


-- *** SHOW SUST *** --



-- *** SUSTITUCION ***
sust' :: Term -> (Sust) -> Term
sust' Truee (Sustit t2 t3) = Truee
sust' Falsee (Sustit t2 t3) = Falsee
sust' (Var i) (Sustit (Var j) (Var k)) = if i==k then (Var j) else (Var i)
sust' (Var i) (Sustit t1 (Var j)) = if i==j then t1 else (Var i)
-- sustitucion para \/
sust' (Or t1 t2) (Sustit (Var i) (Var j)) = Or (sust' t1 ((Var i)=:(Var j))) (sust' t2 ((Var i)=:(Var j)))
sust' (Or t1 t2) (Sustit t3 (Var j)) = Or (sust' t1 (t3=:(Var j))) (sust' t2 (t3=:(Var j)))
--sustitucion para /\
sust' (And t1 t2) (Sustit (Var i) (Var j)) = And (sust' t1 ((Var i)=:(Var j))) (sust' t2 ((Var i)=:(Var j)))
sust' (And t1 t2) (Sustit t3 (Var j)) = And (sust' t1 (t3=:(Var j))) (sust' t2 (t3=:(Var j)))
--sustitucion para ==>
sust' (Imp t1 t2) (Sustit (Var i) (Var j)) = Imp (sust' t1 ((Var i)=:(Var j))) (sust' t2 ((Var i)=:(Var j)))
sust' (Imp t1 t2) (Sustit t3 (Var j)) = Imp (sust' t1 (t3=:(Var j))) (sust' t2 (t3=:(Var j)))
--sustitucion para <==>
sust' (Equiv t1 t2) (Sustit (Var i) (Var j)) = Equiv (sust' t1 ((Var i)=:(Var j))) (sust' t2 ((Var i)=:(Var j)))
sust' (Equiv t1 t2) (Sustit t3 (Var j)) = Equiv (sust' t1 (t3=:(Var j))) (sust' t2 (t3=:(Var j)))
--sustitucion para !<==>
sust' (Inequiv t1 t2) (Sustit (Var i) (Var j)) = Inequiv (sust' t1 ((Var i)=:(Var j))) (sust' t2 ((Var i)=:(Var j)))
sust' (Inequiv t1 t2) (Sustit t3 (Var j)) = Inequiv (sust' t1 (t3=:(Var j))) (sust' t2 (t3=:(Var j)))
--sustitucion para ¬
sust' (Neg t1) (Sustit (Var i) (Var j)) = Neg (sust' t1 ((Var i)=:(Var j)))
sust' (Neg t1) (Sustit t3 (Var j)) = Neg (sust' t1 (t3=:(Var j)))


-- ** COMPARAR EXPRESIONES **
-- comparar lados de una ecuacion
compare_show :: Term -> Equation -> Term
compare_show t1 (Igual t2 t3) = if t1==t2 then t3 
								else if t1==t3 then t2
								else error "Invalid inference rule"


-- *** INSTANCIAR **
instantiate :: Sustitution a => Equation -> a -> Equation
instantiate (Igual t1 t2) a = Igual (sust t1 a) (sust t2 a)


-- *** APLICAR LA REGLA DE LEIBNIZ ***
leibniz :: Equation -> Term -> Term -> Equation
leibniz (Igual t1 t2) var t = Igual (sust t (t1=:var)) (sust t (t2=:var))


-- ** INFERENCIA **
infer :: Sustitution a => Float -> a -> Term -> Term -> Equation
infer num obj_sust z term = leibniz (instantiate (prop num) obj_sust) z term 


-- ** APLICAR TODO ***
step :: Sustitution a => Term -> Float -> a -> Term -> Term -> Term
step t1 num s z term = compare_show t1 (infer num s z term)


-- *** FUNCIONES DE INICIO Y FIN DE LA DEMOSTRACION ***
-- inicio
proof :: Equation -> IO Term
proof (Igual t1 t2) = do
						let x = t1
						putStrLn $ id "prooving "++showEquation(Igual t1 t2)
						putStrLn $ id ""
						print x
						return (x)
-- fin
done :: Equation -> Term -> IO()
done (Igual t1 t2) tf = do
						let x = t2
						if t2==tf 
							then putStrLn $ id "proof successful"
							else putStrLn $ id "proof unsuccessful"


-- *** FUNCION QUE RECIBE LOS ARGUMENTOS DEL HINT ***
statement :: Sustitution a => Float -> Term -> a -> Term -> Term -> Term -> Term -> Term -> IO Term
statement num with (obj_sust) using lambda z zterm ti= do
													let x = step ti num obj_sust z zterm
													putStrLn $ id "===<statement "++show(num)++" with " ++ showSust(obj_sust) ++" using lambda "++showTerm(z)++" ("++showTerm(zterm)++")>"
													print(x)
													return (x)

--hola obj 


-- *** INSTANCE ***
instance Show Term where show = showTerm
instance Show Equation where show = showEquation
-- comparar terminos
instance Eq Term where 	
						Var i == Var j = i == j
						-- comparar true
						Truee == Truee = True
						Truee == _ = False
						_ == Truee = False

						-- comparar false
						Falsee == Falsee = True
						_ == Falsee = False
						Falsee == _ = False

						-- comparar neg
						(Neg t1) == (Neg t2) = t1 == t2
						(Neg t1) == _ = False
						_ == (Neg t1) = False

						-- comparar or
						(Or t1 t2) == (Or t3 t4) = (t1 == t3) && (t2 == t4)
						_ == (Or t3 t4) = False
						(Or t3 t4) == _ = False

						-- comparar and
						(And t1 t2) == (And t3 t4) = (t1 == t3) && (t2 == t4)
						_ == (And t3 t4) = False
						(And t3 t4) == _ = False

						-- comparar imp
						(Imp t1 t2) == (Imp t3 t4) = (t1 == t3) && (t2 == t4)
						_ == (Imp t3 t4) = False
						(Imp t3 t4) == _ = False

						-- comparar equiv
						(Equiv t1 t2) == (Equiv t3 t4) = (t1 == t3) && (t2 == t4)
						_ == (Equiv t3 t4) = False
						(Equiv t3 t4) == _ = False

						-- comparar inequiv
						(Inequiv t1 t2) == (Inequiv t3 t4) = (t1 == t3) && (t2 == t4)
						_ == (Inequiv t3 t4) = False
						(Inequiv t3 t4) == _ = False
-- Instancias de la sustitucion para los tres tipos (simple, doble, triple)
class Sustitution s where
	sust ::Term -> s -> Term
	showSust :: s -> String
	
--instance Show (Sustitution Sust) where show (Sustit t1 t2) = showSust (Sustit t1 t2)
--instance Show (Sustitution (Term,Sust,Term)) where show = "wa"
--instance Show (Sustitution (Term,Term,Sust,Term,Term)) where show = "wa"


 --Mostrar sustitucion


instance Sustitution (Sust) where
	sust t (Sustit t1 t2) = sust' t (t1=:t2)
	showSust(Sustit t1 t2) = "(" ++ showTerm t1 ++ " =: " ++ showTerm t2 ++ ")"

instance Sustitution (Term,Sust,Term) where
	sust t (t1,Sustit t2 t3,t4) = sust' (sust' (sust' t (fresca=:t3)) (t2=:t4)) (t1=:fresca)
	showSust (t1,Sustit t2 t3,t4) = "("++showTerm t1++"," ++ showTerm t2 ++ " =: " ++ showTerm t3 ++ "," ++ showTerm t4 ++ ")"

instance Sustitution (Term,Term,Sust,Term,Term) where
	sust t (t1,t2,Sustit t3 t4,t5,t6) = sust' (sust' (sust' (sust' (sust' t (fresca=:t4)) (fresca'=:t5)) (t3=:t6)) (t1=:fresca)) (t2=:fresca')
	showSust (t1,t2,Sustit t3 t4,t5,t6) = "("++showTerm t1++"," ++ showTerm t2 ++ "," ++ showTerm t3 ++  "=:"  ++ showTerm t4 ++ "," ++ showTerm t5 ++ "," ++ showTerm t6 ++ ")"