module Theorem3_34 where
import Definiciones
import Term
-----------------------------------------
-- Coloque los imports necesarios aqui --
-----------------------------------------

verify = let theorem = ( p \/ (q <==> r) <==> p \/ q === p \/ r ) in
         proof theorem
         >>=
         statement 3.27 with (p =: p) using lambda z (z <==> p \/ q)
         >>=
         statement 3.2 with (p \/ q, p \/ r =: p, q) using lambda z (z <==> p \/ q)
         >>=
         statement 3.5 with (p \/ r, p \/ q =: p, q) using lambda z (z)
         >>=
         done theorem
