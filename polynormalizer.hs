{----------------------------------------------------------

University of Porto

up202202993 - Remus Comeaga
up202006950 - Vicente Lora

----------------------------------------------------------}

module Polynormalizer where

------------------------- Imports -------------------------

import Data.List
import Polytypes (Var, Coef, Exp, Monomial, Polynomial) 
import Polyparser (get_mono_vars, get_mono_coef, get_mono_exps)

--------------------------- Misc ---------------------------

type MonoSort = ([(Var, Exp)], Coef)

numberLength :: Int -> Int  -- Gives the length of an integer
numberLength 0 = 0
numberLength x = 1 + numberLength (x `div` 10)

dec2int :: [Int] -> Int -- Converts list of integers to number
dec2int = foldl (\ x y -> (10^numberLength y) * x + y) 0

sorted :: (Ord a) => [a] -> Bool
sorted [] = True
sorted [x] = True
sorted (x:y:xs) = if x <= y then sorted (y:xs) else False

-------------------- Monomial Operations -------------------

isJoinable :: Monomial -> Monomial -> Bool -- Verifies if monomials are joinable by summing
isJoinable x y | get_mono_vars x == get_mono_vars y && get_mono_exps x == get_mono_exps y = True
               | otherwise = False

isMult :: (Var, Exp) -> (Var, Exp) -> Bool
isMult x y | fst x == fst y = True
           | otherwise = False

sumJoinMono :: [Monomial] -> Monomial -- Sums joinable monomials
sumJoinMono [x] = x
sumJoinMono [x1, x2] = (get_mono_vars x1, get_mono_coef x1 + get_mono_coef x2, get_mono_exps x1)
sumJoinMono (x1:x2:xs) = sumJoinMono ([(get_mono_vars x1, get_mono_coef x1 + get_mono_coef x2, get_mono_exps x1)] ++ xs)

mono2sort :: Monomial -> MonoSort
mono2sort (v, c, e) = (zip v e, c)

sort2mono :: MonoSort -> Monomial
sort2mono (x, e) = ([v | (v, _) <- x], e, [c | (_, c) <- x])

cmpMonoVar :: Ord a => (a, b) -> (a, b) -> Ordering
cmpMonoVar x y | fst x < fst y = LT
            | fst x == fst y = EQ
            | otherwise = GT

sortMono :: MonoSort -> MonoSort
sortMono (x, e) = (sortBy cmpMonoVar x, e)

multJoinVar :: [(Var, Exp)] -> (Var, Exp)
multJoinVar x = (fst (head x), sum [snd y | y <- x ])

multVar :: MonoSort -> MonoSort
multVar (x, e) = (map multJoinVar (groupBy isMult x), e)

------------------- Polynomial operations -------------------

-- Sorts the polynomial based on the variables, alphabetically
sortPolyVar :: Polynomial -> Polynomial
sortPolyVar [] = []
sortPolyVar (x:xs) = sortPolyVar left ++ [x] ++ sortPolyVar right
    where
    left = [y | y <- xs, concat (get_mono_vars y) <= concat (get_mono_vars x)]
    right = [y | y <- xs, concat (get_mono_vars y) > concat (get_mono_vars x)]

-- Sorts polynomial based on the exponents
sortPolyExp :: Polynomial -> Polynomial
sortPolyExp [] = []
sortPolyExp (x:xs) = sortPolyExp left ++ [x] ++ sortPolyExp right
    where
    left = [y | y <- xs,  dec2int (get_mono_exps y) >= dec2int (get_mono_exps x)]
    right = [y | y <- xs, dec2int (get_mono_exps y) < dec2int (get_mono_exps x)]

-- Removes monomials with the coeficient 0
removeNullCoef :: Polynomial -> Polynomial
removeNullCoef p = [x | x <- p, get_mono_coef x /= 0]

-- Removes variables with the exponent 0 from the monomial
removeNullExpFromMono :: Monomial -> Monomial
removeNullExpFromMono m = (newVars, coef, newExps)
    where vars = get_mono_vars m
          coef = get_mono_coef m
          exps = get_mono_exps m
          whereRemove = [ i | (e,i) <- zip exps [0..], e == 0]
          newVars = [v | (v, i) <- zip vars [0..], not (elem i whereRemove)]
          newExps = [e | (e, i) <- zip exps [0..], not (elem i whereRemove)]

-- Removes variable with the exponent 0 from the polynomial
removeNullExp :: Polynomial -> Polynomial
removeNullExp p = [removeNullExpFromMono m | m <- p]

-- Normalizes a polynomial
normalizePoly :: Polynomial -> Polynomial
normalizePoly x = removeNullExp (removeNullCoef joinedpoly)
    where
        joinedpoly = map sumJoinMono sortedPolyExp
            where
                sortedPolyExp = groupBy isJoinable sortedPolyExp
                    where
                        sortedPolyExp = concat (map sortPolyExp sortedPolyVar)
                            where
                                sortedPolyVar = groupBy (\ x y -> get_mono_vars x == get_mono_vars y) (sortPolyVar joinedMono)
                                    where 
                                        joinedMono = map sort2mono (map multVar sortedMono)
                                            where
                                                sortedMono = map sortMono (map mono2sort (removeNullCoef x))