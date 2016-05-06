import abecedario 
--data
data Term = Var Char
--data Sust = Term

--operadores
infix 1 \/
(\/) :: Term -> Term -> Bool
x \/ y = x || y
infix 1 /\
(/\) :: Term -> Term -> Bool
x /\ y = x && y
--infix 3 ==>
--infix 4 <==>
--infix 5 !<==>
--infix 6 ===
--infix 9 =:
--(=:) :: Term -> Term -> Sust
--a := b = sust exp (a,b)
--sustitucion
--sust :: Term -> (Term,Term) -> Term
--sust (Var s1) (Var s2, term2) = if s1==s2 then term2 else (Var s1)
--sust (Sum term1 term2) (var,term) = Sum (sust term1 (var,term)) (sust term2 (var,term))