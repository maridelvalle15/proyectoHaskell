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