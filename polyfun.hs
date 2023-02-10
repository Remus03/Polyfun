{----------------------------------------------------------

University of Porto

up202202993 - Remus Comeaga
up202006950 - Vicente Lora

-----------------------------------------------------------

This is the application format of the polyfun app

To run it please follow this steps:
1. Open the terminal on the same folder as "polyfun.hs"
2. Use the command "ghc .\polyfun.hs"
3. Wait until the compilation is finished
4. Run the newly created "polyfun.exe" application

After running the application, just follow the prompts 
(Please make sure to be careful with your inputs)

------------------------- Imports ------------------------}

import Polyparser (build_polynomial, build_polynomials)
import Polynormalizer (normalizePoly)
import Polyadder (addPoly, addPolys)
import Polymultiplier (multiplyPoly, multiplyPolys)
import Polyderiver (derivePolynomial)
import Polyoutput (prepare_output)

------------------------------------------------------------

main :: IO ()
main = 
    do
        -- Show a menu with the possible operations
        op <- askOperation
        -- User picks an operation that takes only one polynomial (normalization or derivation)
        if op == "1" || op == "4" 
            then
                do
                    poly <- askForPolynomial
                    -- If the operation is normalization
                    if op == "1" 
                    then 
                        do 
                            putStrLn (prepare_output (normalizePoly (build_polynomial poly)))
                            -- Ask the user to press enter to go back to the menu
                            continue <- keepScreen
                            -- Go back to the menu
                            main
                    -- If the operation is derivation
                    else        
                        do 
                            var <- askVarDerivation
                            putStrLn (prepare_output (normalizePoly (derivePolynomial (var) ((normalizePoly (build_polynomial poly))))))
                            continue <- keepScreen
                            main
        -- User picks to exit the app
        else   
            if op == "Q" || op == "q" 
            then
                do
                    putStr ("Exiting...\n")
            -- User picks an operation that takes 2 or more polynomials (addition or multiplication)
            else
                do
                    polysInput <- askForPolynomials
                    -- If the operation is addition
                    if op == "2"
                    then
                        do
                            putStrLn (prepare_output (addPolys (build_polynomials polysInput)))
                            continue <- keepScreen
                            main
                    -- If the operation is multiplication
                    else
                        do
                            putStrLn (prepare_output (multiplyPolys (build_polynomials polysInput)))
                            continue <- keepScreen
                            main

-- Ask the user what app function they want to use (normalize, add, multiply, derive, exit)
askOperation :: IO String
askOperation =
    do
        putStr ("What operaion would you like to do:\n1 - Normalization\n2 - Addition\n3 - Multiplication\n4 - Derivation\nQ - Exit the programme\n\n")
        op <- getLine
        if op == "1" || op == "2" || op == "3" || op == "4" || op == "Q" || op == "q" then return op
        else do
            putStrLn ("Invalid option (Make sure to enter one of the options shown and avoid adding any space in the end)\n\n")
            askOperation

-- Ask for the polynomials the user wants to do either derive or normalize
askForPolynomial :: IO String
askForPolynomial =
    do
        putStr ("Please input your polynomial\nOperations should be defined as follows:\n * - multiplication (ex: 2 * x)\n ^ - exponent (ex: x^2)\n + - addition (2 + 3)\n\n")
        poly <- getLine
        return poly

-- Ask for the polynomials the user wants to do either multiplication or addition to
askForPolynomials :: IO String
askForPolynomials =
    do
        putStr ("Please input your polynomials separated by commas ','\nOperations should be defined as follows:\n * - multiplication (ex: 2 * x)\n ^ - exponent (ex: x^2)\n + - addition (2 + 3)\n\n")
        polys <- getLine
        return polys

-- Ask for the variable the user wants to derive the polynomial in order to
askVarDerivation :: IO String
askVarDerivation =
    do
        putStr ("What variable would you like to deriver the polynomial in order to?\n\n")
        var <- getLine
        return var

-- Keep screen until user presses enter to continue
keepScreen :: IO String
keepScreen =
    do
        putStr ("Press enter to go back to the menu\n\n")
        continue <- getLine
        return continue