module Theorems3_4 where
import Definiciones
import Abecedario
import System.IO  
import Control.Monad

verify = let theorem = (true === ((p<==>p) <==> (q <==> q))) in 
		statement 3.2 with (p=:p) using lambda z