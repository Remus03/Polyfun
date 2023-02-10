{----------------------------------------------------------

University of Porto

upxxxxxxxxx - Remus Comeaga
up202006950 - Vicente Lora

----------------------------------------------------------}

module Polytypes where

--------------------- Types Definition ---------------------

type Var = String
type Coef = Int
type Exp = Int

type Monomial = ([Var], Coef, [Exp])
type Polynomial = [Monomial]

------------------------------------------------------------