{----------------------------------------------------------

University of Porto

up202202993 - Remus Comeaga
up202006950 - Vicente Lora

----------------------------------------------------------}

module Polyframework where

{-----------------------------------------------------------

This is the ghci format of the polyfun app

To run it please follow this steps:
1. Open the terminal on the same folder as "polyframework.hs"
2. Use the command "ghci"
3. When the ghci opens, use the command ":l polyframework.hs"

After loading the file, feel free to use the following available commands

------------------------- Imports -------------------------}

import Polytypes (
                    Var, ---------------- = String
                    Coef, --------------- = Int
                    Exp, ---------------- = [Int]
                    Monomial, ----------- = ([Var], Coef, [Exp])
                    Polynomial) --------- = [Monomial]

import Polyparser (
                    build_polynomial, --- - Converts Polynomial written as String into Internal form ---- (String -> Polynomial)
                    build_polynomials, -- - Converts Polynomials separated by commas into Internal form - (String -> [Polynomial])
                    get_mono_from, ------ - Gets the monomial at index i in a polynomial ---------------- (Int -> Polynomial -> Monomial)
                    get_mono_vars, ------ - Gets the List of Variables of a certain monomial ------------ (Monomial -> [Var])
                    get_mono_coef, ------ - Gets the Coefficient of a certain monomial ------------------ (Monomial -> Coef)
                    get_mono_exps, ------ - Gets the List of Exponents of a certain monomial ------------ (Monomial -> [Exp])
                    get_poly_vars, ------ - Gets a list of every monomials' List of Variables ----------- (Polynomial -> [[Var]])
                    get_poly_coefs, ----- - Gets a list of every monomials' Coefficient ----------------- (Polynomial -> [Coef])
                    get_poly_exps, ------ - Gets a list of every monomials' List of Exponents ----------- (Polynomial -> [[Exp]])
                    get_poly_size) ------ - Gets the amount of monomials in a polynomial ---------------- (Polynomial -> Int)

import Polynormalizer (
                    normalizePoly, ------ - Normalizes a polynomial ------------------------------------- (Polynomial -> Polynomial)
                    sortPolyVar, -------- - Sorts the polynomial based on the variables, alphabetically - (Polynomial -> Polynomial)
                    sortPolyExp, -------- - Sorts polynomial based on the exponents --------------------- (Polynomial -> Polynomial)
                    removeNullCoef) ----- - Removes monomials with the coeficient 0 --------------------- (Polynomial -> Polynomial)

import Polyadder (
                    addPoly, ------------ - Adds 2 Polynomials ------------------------------------------ (Polynomial -> Polynomial -> Polynomial)
                    addPolys) ----------- - Adds many Polynomials --------------------------------------- ([Polynomial] -> Polynomial)

import Polymultiplier (
                    multiplyPoly, ------- - Multiply 2 Polynomials -------------------------------------- (Polynomial -> Polynomial -> Polynomial)
                    multiplyPolys) ------ - Multiply many Polynomials ----------------------------------- ([Polynomial] -> Polynomial))

import Polyderiver (
                    deriveMonomial, ----- - Derivates monomial in order to a certain variable ----------- (Var -> Monomial -> Monomial)
                    derivePolynomial) --- - Derivates polynomial in order to a certain variable --------- (Var -> Polynomial -> Polynomial)

import Polyoutput (
                    prepare_output) ----- - Convert a Polynomial in Internal form to String form -------- (Polynomial -> String)

------------------------------------------------------------

-- The following functions allow you to skip the process of 
-- converting each string to internal form by hand or the other
-- way around:

normalizeStringPoly :: String -> String
normalizeStringPoly str = prepare_output (normalizePoly (build_polynomial str))

addStringPoly :: String -> String -> String
addStringPoly s1 s2 = prepare_output (addPoly (build_polynomial s1) (build_polynomial s2))

addStringPolys :: String -> String
addStringPolys strs = prepare_output (addPolys (build_polynomials strs))

multiplyStringPoly :: String -> String -> String
multiplyStringPoly s1 s2 = prepare_output (multiplyPoly (build_polynomial s1) (build_polynomial s2))

multiplyStringPolys :: String -> String
multiplyStringPolys strs = prepare_output (multiplyPolys (build_polynomials strs))

deriveStringPolynomial :: Var -> String -> String
deriveStringPolynomial var str = prepare_output (derivePolynomial var (build_polynomial str))