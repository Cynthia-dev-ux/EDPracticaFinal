data Var = A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z deriving (Show, Eq, Ord)

data Formula = Atom Var
               |Neg Formula
               |Formula :&: Formula
               |Formula :|: Formula
               |Formula :=>: Formula
               |Formula :<=>: Formula deriving (Show, Eq, Ord)

infixl 9 :&:
infixl 9 :|:
infixl 7 :=>:
infixl 8 :<=>:


-------------------- EJERCICIO 1 --------------------
conjunto :: Eq a => [a] -> [a]
conjunto [] = []
conjunto (x:xs) = x conjunto [y|y = xs, y = x]

variables :: Formula -> [Var]
variables (Atom var) = [var] 
variables(Neg formula) = conjunto (variables formula)
variables (formula_1 = formula 2) = conjunto (variables formula_1 variables formula_2)

-----------------------------------------------------

-------------------- EJERCICIO 2 --------------------
negacion :: Formula -> Formula
negacion _ = undefined
-----------------------------------------------------

-------------------- EJERCICIO 3 --------------------
equivalencia :: Formula -> Formula
equivalencia _ = undefined
-----------------------------------------------------

-------------------- EJERCICIO 4 --------------------
interpretacion :: Formula -> [(Var,Bool)] -> Bool
interpretacion _ = undefined
-----------------------------------------------------

-------------------- EJERCICIO 5 --------------------
combinaciones :: Formula -> [[(Var,Bool)]]
combinaciones _ = undefined
-----------------------------------------------------

-------------------- EJERCICIO 6 --------------------
tablaDeVerdadCom :: Formula -> [[Var,Bool]] -> [([(Var,Bool)],Bool)]
tablaDeVerdadCom formula [] = []
tablaDeVerdadCom formula (x:xs) = (x,interpretacion formula x)
                     :(tablaDeVerdadCom formula xs)



tablaDeVerdad :: Formula -> [([(Var,Bool)],Bool)]
tablaDeVerdad formula = tablaDeVerdad formula (combinaciones formula)
-----------------------------------------------------



