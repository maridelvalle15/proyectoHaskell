module Theorem3_33 where
import Definiciones
import Term
-----------------------------------------
-- Coloque los imports necesarios aqui --
-----------------------------------------

verify = let theorem = ( p \/ q <==> p \/ neg q === p ) in
         proof theorem
         >>=
         statement 3.32 with (p =: p) using lambda z (z <==> p \/ neg q)
         >>=
         statement 3.2 with (p \/ neg q, p =: p, q) using lambda z (z <==> p \/ neg q)
         >>=
         statement 3.5 with (p \/ neg q =: q) using lambda z (z)
         >>=
         done theorem
