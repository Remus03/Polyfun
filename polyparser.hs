{----------------------------------------------------------

University of Porto

up202202993 - Remus Comeaga
up202006950 - Vicente Lora

----------------------------------------------------------}

module Polyparser where

------------------------- Imports -------------------------

import Data.List
import Polytypes (Var, Coef, Exp, Monomial, Polynomial)

-------------------- Raw String Handling -------------------

-- Remove any spaces from the String Input
clean :: String -> String
clean str = filter (/= ' ') str

-- Replaces every instance of a character in a String with a given String
replace_char :: Char -> String -> String -> String
replace_char _ _ [] = []
replace_char c1 s2 (c:str)
    | c == c1 = s2 ++ replace_char c1 s2 str 
    | otherwise = [c] ++ replace_char c1 s2 str

-- Replaces every instance of a character with a space " "
replace_with_space :: Char -> String -> String
replace_with_space c str = replace_char c " " str

--------------------- String Searching ---------------------

-- Find the position of the indicators of exponentiation '^'
find_power :: String -> [Int]
find_power str = [i | (c, i) <- zip str [0..], c == '^']

-- Find the position of the indicator of multiplication '*'
find_multiplication :: String -> [Int]
find_multiplication str = [i | (c, i) <- zip str [0..], c == '*']

-------------------- Monomial Building --------------------

-- Adds a plus sign before every minus sign in a String
add_plus_signs :: String -> String
add_plus_signs str = replace_char '-' "+-" str

-- Separates a Polynomial String into its Monomials Strings
get_monomials_strings :: String -> [String]
get_monomials_strings "" = []
get_monomials_strings str = words (replace_with_space '+' (add_plus_signs (clean str)))

-- Separates monomial components
separate_components :: String -> [String]
separate_components str = words (replace_with_space '*' str)

-- Checks if there's any exponent indicator in a String
hasExponent :: String -> Bool
hasExponent str = '^' `elem` str

-- Gets 
separate_var_exp :: String -> (Var, Exp)
separate_var_exp str = (var, expo)
    where sep = (find_power str) !! 0
          var = take sep str
          expo = read (drop (sep + 1) str) :: Int

-- Find the variables of the monomial
find_mono_var_exp :: [String] -> ([Var], [Exp])
find_mono_var_exp comps
    | null non_coefs = ([], [])
    | otherwise = (vars , exps)
    where non_coefs = filter (not . isCoef) comps
          non_powered = filter (not . hasExponent) non_coefs
          powered = filter (hasExponent) non_coefs
          vars_exps = map separate_var_exp powered
          just_vars = [fst var_exp | var_exp <- vars_exps]
          just_exps = [snd var_exp | var_exp <- vars_exps]
          vars = non_powered ++ just_vars
          cleaned_vars = map (replace_char '-' "") vars
          exps = (replicate (length non_powered) 1) ++ just_exps

-- Checks if a component is a coefficient
isCoef :: String -> Bool
isCoef str = all (`elem` "-0123456789") str

-- Checks if a component is not a coefficient
notIsCoef :: String -> Bool
notIsCoef str = not (isCoef str)

-- Find the coefficient of the monomial
find_mono_coef :: [String] -> Int
find_mono_coef comps
    -- if there isn't any shown coefficient and the monomial is negative
    | null coefs && (elem '-' (head comps)) = -1
    -- if there isn't any shown coefficient and the monomial is positive
    | null coefs = 1
    -- if the coefficient is negative
    | (elem '-' (head comps)) && not (head comps == head coefs) = -1 * coef
    -- if the coefficient is potive and is shown
    | otherwise = coef
    where coefs = filter (isCoef) comps
          coef = product (map (read :: String -> Int) coefs)

-- Find the exponents of the monomial
find_mono_exps :: [String] -> [Int]
find_mono_exps mono = undefined

-- Builds a Monomial by joining its components (variables, coefficient and exponents)
build_monomial :: [String] -> Monomial
build_monomial strs = (vars, coef, exps)
    where vars_exps = find_mono_var_exp strs
          vars = fst vars_exps
          exps = snd vars_exps
          coef = find_mono_coef strs

------------------------------------------------------------
-------------------- Framework Starts Here -----------------
------------------------------------------------------------

-- Converts Polynomial written as String into Internal form
build_polynomial :: String -> Polynomial
build_polynomial str = map build_monomial (map separate_components (get_monomials_strings str))

-- Converts Polynomials written as String, and separated by commas, into Internal form
build_polynomials :: String -> [Polynomial]
build_polynomials str = [build_polynomial s | s <- separated_str]
    where separated_str = words (replace_with_space ',' (clean str))

--------------------- Componet Getting ---------------------

-- Gets the List of Variables of a certain monomial
get_mono_vars :: Monomial -> [Var]
get_mono_vars (vars,_,_) = vars

-- Gets the Coefficient of a certain monomial
get_mono_coef :: Monomial -> Coef
get_mono_coef (_,coef,_) = coef

-- Gets the List of Exponents of a certain monomial
get_mono_exps :: Monomial -> [Exp]
get_mono_exps (_,_,expos) = expos

--------------------- Monomial Getting ---------------------

-- Gets the monomial at index i in a polynomial
get_mono_from :: Int -> Polynomial -> Monomial
get_mono_from i poly = poly !! i

-------------------- Polynomial Getting --------------------

-- Gets a list of every monomials' List of Variables
get_poly_vars :: Polynomial -> [[Var]]
get_poly_vars poly = [vars | (vars,_,_) <- poly]

-- Gets a list of every monomials' Coefficient
get_poly_coefs :: Polynomial -> [Coef]
get_poly_coefs poly = [coef | (_,coef,_) <- poly]

-- Gets a list of every monomials' List of Exponents
get_poly_exps :: Polynomial -> [[Exp]]
get_poly_exps poly = [exps | (_,_,exps) <- poly]

-- Gets the amount of monomials in a polynomial
get_poly_size :: Polynomial -> Int
get_poly_size poly = length poly

------------------------------------------------------------
------------------------------------------------------------
------------------------------------------------------------