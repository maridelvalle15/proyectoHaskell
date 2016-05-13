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
--sustitucion para ¬
sust (Neg t1) (Sustit (Var i) (Var j)) = Neg (sust t1 ((Var i)=:(Var j)))
sust (Neg t1) (Sustit t3 (Var j)) = Neg (sust t1 (t3=:(Var j)))

--sustitucion para tuplas
--sust t1 (SustitDos (t2,(Sustit (Var j) (Var k)),t4)) = sust t1 ((Var j)=:(Var k))

-- *** SHOW TERMS *** --sust (sust (t1 (k j)))
showTerm :: Term -> String
showTerm(Var i) = [i]

--Mostrar ¬
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
showTerm(Imp (Neg t1)(Neg t2)) = showTerm(Neg t1) ++ " ==> " ++ showTerm(Neg t2)
showTerm(Imp (Neg t) t1) = showTerm(Neg t) ++ " ==> " ++ showTerm(t1)
showTerm(Imp t1 (Neg t)) = showTerm(t1) ++ " ==> " ++ showTerm(Neg t) 
showTerm (Neg (Imp t1 t2)) = "¬(" ++ showTerm (Imp t1 t2) ++ ")"
showTerm(Imp (Var i) (Neg t)) = showTerm(Var i) ++ " ==> " ++ showTerm(Neg t)
showTerm(Imp (Neg i)(Var j)) = showTerm(Neg i) ++ " ==> " ++ showTerm(Var j)
 --Mostrar ¬ <==>
showTerm(Equiv (Neg t1)(Neg t2)) = showTerm(Neg t1) ++ " <==> " ++ showTerm(Neg t2)
showTerm(Equiv (Neg t) t1) = showTerm(Neg t) ++ " <==> " ++ showTerm(t1)
showTerm(Equiv t1 (Neg t)) = showTerm(t1) ++ " <==> " ++ showTerm(Neg t) 
showTerm (Neg (Equiv t1 t2)) = "¬(" ++ showTerm (Equiv t1 t2) ++ ")"
showTerm(Equiv (Var i) (Neg t)) = showTerm(Var i) ++ " <==> " ++ showTerm(Neg t)
showTerm(Equiv (Neg i)(Var j)) = showTerm(Neg i) ++ " <==> " ++ showTerm(Var j)
 --Mostrar ¬ !<==>
showTerm(Inequiv (Neg t1)(Neg t2)) = showTerm(Neg t1) ++ " !<==> " ++ showTerm(Neg t2)
showTerm(Inequiv (Neg t) t1) = showTerm(Neg t) ++ " !<==> " ++ showTerm(t1)
showTerm(Inequiv t1 (Neg t)) = showTerm(t1) ++ " !<==> " ++ showTerm(Neg t) 
showTerm (Neg (Inequiv t1 t2)) = "¬(" ++ showTerm (Inequiv t1 t2) ++ ")"
showTerm(Inequiv (Var i) (Neg t)) = showTerm(Var i) ++ " !<==> " ++ showTerm(Neg t)
showTerm(Inequiv (Neg i)(Var j)) = showTerm(Neg i) ++ " !<==> " ++ showTerm(Var j)


showTerm (Neg t1) = "¬(" ++ showTerm t1 ++ ")"

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
--Mostrar true y false
showTerm(Truee) = "true"
showTerm(Falsee) = "false"

-- *** SHOW SUST *** --
showSust :: Sust -> String
--Mostrar sustitucion
showSust(Sustit t1 t2) = showTerm t1 ++ " =: " ++ showTerm t2

-- *** SHOW EQUATION *** --
showEquation :: Equation -> String
--Mostrar sustitucion
showEquation(Igual t1 t2) = showTerm t1 ++ " === " ++ showTerm t2

-- *** INSTANCE ***
instance Show Term where show = showTerm
instance Show Sust where show = showSust
instance Show Equation where show = showEquation

instance Eq Term where 	
						Var i == Var j = i == j
						Truee == Truee = True
						Falsee == Falsee = True
						(Neg t1) == (Neg t2) = t1 == t2
						(Or t1 t2) == (Or t3 t4) = (t1 == t3) && (t2 == t4)
						(And t1 t2) == (And t3 t4) = (t1 == t3) && (t2 == t4)
						(Imp t1 t2) == (Imp t3 t4) = (t1 == t3) && (t2 == t4)
						(Equiv t1 t2) == (Equiv t3 t4) = (t1 == t3) && (t2 == t4)
						(Inequiv t1 t2) == (Inequiv t3 t4) = (t1 == t3) && (t2 == t4)
						