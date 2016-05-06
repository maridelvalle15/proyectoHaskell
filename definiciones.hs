import Abecedario 

--terminos validos
true :: Term
true = Truee
false :: Term
false = Falsee

--operadores
infix 1 neg
(neg) :: Term -> Term
infix 2 \/
(\/) :: Term -> Term -> Term
x \/ y = Or x y
infix 2 /\
(/\) :: Term -> Term -> Term
x /\ y = And x y
infix 3 ==>
(==>) :: Term -> Term -> Term
x ==> y = Imp term Term
infix 4 <==>
--(<==>) :: Term -> Term -> Term
x <==> y =  Equiv x y
infix 5 !<==>
--(<==>) :: Term -> Term -> Term
x !<==> y =  Inequiv x y
infix 6 ===
(===) :: Term -> Term -> Equation

--infix 9 =:

{-
(=:) :: Term -> Term -> Sust     
a =: b = sust exp (a,b)
-}
{-
--sustitucion
--sust :: Term -> (Term,Term) -> Term
--sust (Var s1) (Var s2, term2) = if s1==s2 then term2 else (Var s1)
--sust (Sum term1 term2) (var,term) = Sum (sust term1 (var,term)) (sust term2 (var,term))
-}