-- Define predicates and, or, nand, nor, xor, impl and equ
-- which succeed or fail according to the result of their respective operations.
not' :: Bool -> Bool
not' True = False
not' False = True

and' :: Bool -> Bool -> Bool
and' True True = True
and' _ _ = False

or' :: Bool -> Bool -> Bool
or' False False = False
or' _ _ = True

nand' :: Bool -> Bool -> Bool
nand' x y = not' $ and' x y

nor' :: Bool -> Bool -> Bool
nor' x y = not' $ or' x y

equ' :: Bool -> Bool -> Bool
equ' x y
    | x == y    = True
    | otherwise = False

xor' :: Bool -> Bool -> Bool
xor' x y = not' $ equ' x y

impl' :: Bool -> Bool -> Bool
impl' x y = or' (not' x) y

-- Prints the truth table of a given logical expression in two variables.
table :: (Bool -> Bool -> Bool) -> IO ()
table fn = mapM_ (putStrLn . rowToString) (tableRaw fn)

-- HELPER FUNCTIONS
-- Generates the truth table of a given logical expression in two variables.
tableRaw :: (Bool -> Bool -> Bool) -> [(Bool, Bool, Bool)]
tableRaw fn = map (eval fn) params
    where params = [(x, y) | x <- [True, False], y <- [True, False]]
          eval fn (x, y) = (x, y, fn x y)

-- Returns a table row as a string
rowToString :: (Bool, Bool, Bool) -> String
rowToString (a, b, c) = show a ++ " " ++ show b ++ " -> " ++ show c
