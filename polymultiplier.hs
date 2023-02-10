{----------------------------------------------------------

University of Porto

up202202993 - Remus Comeaga
up202006950 - Vicente Lora

----------------------------------------------------------}

module Polymultiplier where

------------------------- Imports -------------------------

import Polytypes (Polynomial)
import Polyparser (get_mono_vars, get_mono_coef, get_mono_exps)
import Polynormalizer (normalizePoly)

------------------------------------------------------------

-- Multiply 2 polynomials
multiplyPoly :: Polynomial -> Polynomial -> Polynomial
multiplyPoly x y = normalizePoly [((get_mono_vars a) ++ (get_mono_vars b), (get_mono_coef a) * (get_mono_coef b), get_mono_exps a ++ get_mono_exps b) | a <- x, b <-y]

-- Multiply many polynomials
multiplyPolys :: [Polynomial] -> Polynomial
multiplyPolys (p:polys)
    | length (p:polys) == 1 = p
    | length polys == 1 = multiplyPoly p (head polys)
    | otherwise = multiplyPoly p (multiplyPolys polys)