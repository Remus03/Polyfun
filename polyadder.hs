{----------------------------------------------------------

University of Porto

up202202993 - Remus Comeaga
up202006950 - Vicente Lora

----------------------------------------------------------}

module Polyadder where

------------------------- Imports -------------------------

import Data.List
import Polytypes (Polynomial)
import Polynormalizer 

------------------------------------------------------------

-- Adds 2 polynomials
addPoly :: Polynomial -> Polynomial -> Polynomial
addPoly x y = normalizePoly (x ++ y)

-- Adds many polynomials
addPolys :: [Polynomial] -> Polynomial
addPolys polys = normalizePoly (concat polys)