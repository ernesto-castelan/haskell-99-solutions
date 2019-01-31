import Data.List

-- Define predicates and, or, nand, nor, xor, impl and equ (short version)
not' :: Bool -> Bool
not' = not

and' :: Bool -> Bool -> Bool
and' = (&&)

or' :: Bool -> Bool -> Bool
or' = (||)

equ' :: Bool -> Bool -> Bool
equ' = (==)

nand' :: Bool -> Bool -> Bool
nand' x y = not' $ and' x y

nor' :: Bool -> Bool -> Bool
nor' x y = not' $ or' x y

xor' :: Bool -> Bool -> Bool
xor' x y = not' $ equ' x y

impl' :: Bool -> Bool -> Bool
impl' x y = or' (not' x) y

-- Prints the truth table of a given logical expression in n variables.
tableN :: Integer -> ([Bool] -> Bool) -> IO ()
tableN n fn = mapM_ (putStrLn . rowToString) (tableRawN n fn)

-- HELPER FUNCTIONS
-- Generates all n-tuples (as lists) of boolean values
paramGen :: Integer -> [[Bool]]
paramGen 0 = [[]]
paramGen n = map (True:) partial ++ map (False:) partial
    where partial = paramGen $ n - 1

-- Generates the truth table of a logical expression in n variables
tableRawN :: Integer -> ([Bool] -> Bool) -> [([Bool], Bool)]
tableRawN n fn = map (eval fn) (paramGen n)
    where eval fn p = (p, fn p)

-- Returns a table row as a string
rowToString :: ([Bool], Bool) -> String
rowToString (p, r) = intercalate " " (map show p) ++ " -> " ++ show r