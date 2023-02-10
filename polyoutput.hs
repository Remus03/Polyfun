{----------------------------------------------------------

University of Porto

up202202993 - Remus Comeaga
up202006950 - Vicente Lora

----------------------------------------------------------}

module Polyoutput where

------------------------- Imports -------------------------

import Data.List
import Polytypes (Var, Exp, Monomial, Polynomial)
import Polyparser (get_mono_vars, get_mono_coef, get_mono_exps)

------------------------------------------------------------

-- Prepares the variables for the output, avoiding useless exponents
add_vars :: ([Var],[Exp]) -> [String]
add_vars ([],[]) = []
add_vars ((v:vs), (e:es))
    | e == 1 = [v] ++ add_vars (vs,es)
    | otherwise = [v ++ "^" ++ show e] ++ add_vars (vs,es)

-- Convert a Monomial in Internal form to String form
convert_monomial :: Monomial -> String
convert_monomial mono
    | vars == [] = show coef
    | coef == 1 = (intercalate "*" (add_vars vars_exps))
    | otherwise = show coef ++ "*" ++ (intercalate "*" (add_vars vars_exps))
    where vars = get_mono_vars mono
          coef = get_mono_coef mono
          exps = get_mono_exps mono
          vars_exps = (vars,exps)

-- Convert a Polynomial in Internal form to String form by converting each monomial and then adding them together
prepare_output :: Polynomial -> String
prepare_output poly = intercalate " + " [convert_monomial mono | mono <- poly]