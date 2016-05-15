module Theorems3_4 where
import Definiciones
import Abecedario

verify = let theorem = (true === ((p<==>p) <==> (q <==> q))) in 
		statement 3.2 with (p=:p) using lambda z