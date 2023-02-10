{----------------------------------------------------------

University of Porto

up202202993 - Remus Comeaga
up202006950 - Vicente Lora

----------------------------------------------------------}

module Polyderiver where

------------------------- Imports -------------------------

import Polytypes (Var, Monomial, Polynomial)
import Polyparser (get_mono_vars, get_mono_coef, get_mono_exps)
import Polynormalizer (normalizePoly)

------------------- Derivation Framework -------------------

-- Derivates monomial in order to a certain variable
deriveMonomial :: Var -> Monomial -> Monomial
deriveMonomial var mono
    -- The derivative of a constant is equal to 0
    | null vars = ([], 0, [])
    -- If the vaiable we want to derivate in order to is not present in the monomial
    | not (var `elem` vars) = (vars, coef, exps)
    -- The derivative of f(x) = ax^n is f'(x) = a*n*x^(n-1)
    | otherwise = (vars, (coef * var_exp), new_exps)
    where vars = get_mono_vars mono
          -- Get the index of the variable
          var_i = [i | (v, i) <- zip vars [0..num_vars], v == var] !! 0
          coef = get_mono_coef mono -- (a)
          exps = get_mono_exps mono
          -- Get the variable variable's correspondent exponent (n)
          var_exp = exps !! var_i
          -- Subtract one from the exponent of the variable we're deriving in order to (n-1)
          new_exps = [ if i == var_i then e - 1 else e | (e, i) <- zip exps [0..num_vars]]
          num_vars = length vars

-- Derivates polynomial in order to a certain variable
derivePolynomial :: Var -> Polynomial -> Polynomial
-- Derivate each monomial one by one, eliminating any equal to zero
derivePolynomial var poly = normalizePoly [deriveMonomial var mono | mono <- poly, get_mono_coef mono /= 0]