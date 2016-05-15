module Abecedario where 
data Term = Var Char | Neg Term |Or Term Term | And Term Term | Imp Term Term |
			Equiv Term Term | Inequiv Term Term | Truee | Falsee
data Equation = Igual Term Term
data Sust = Sustit Term Term


a:: Term
a = Var 'a'

b:: Term
b = Var 'b'

c:: Term
c = Var 'c'

d:: Term
d = Var 'd'

e:: Term
e = Var 'e'

f:: Term
f = Var 'f'

g:: Term
g = Var 'g'

h:: Term
h = Var 'h'

i:: Term
i = Var 'i'

j:: Term
j = Var 'j'

k:: Term
k = Var 'k'

l:: Term
l = Var 'l'

m:: Term
m = Var 'm'

n:: Term
n = Var 'n'

o:: Term
o = Var 'o'

p:: Term
p = Var 'p'

q:: Term
q = Var 'q'

r:: Term
r = Var 'r'

s:: Term
s = Var 's'

t:: Term
t = Var 't'

u:: Term
u = Var 'u'

v:: Term
v = Var 'v'

w:: Term
w = Var 'w'

x:: Term
x = Var 'x'

y:: Term
y = Var 'y'

z:: Term
z = Var 'z'

--Variables frescas para evitar problemas en sustitucion simultanea (ejemplo: (a\/b)[a,b:=b,a])
fresca :: Term
fresca = Var 'A'

fresca' :: Term
fresca' = Var 'B'

with :: Term
with = Var 'Z'
using :: Term
using = Var 'X'
lambda :: Term
lambda = Var 'Y'

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