-- We have to define a new data type, because lists in Haskell are homogeneous.
data ListEncoding a = Single a | Multiple Int a deriving (Show)

-- Run-length encoding of a list (direct solution).
encodeDirect :: (Eq a) => [a] -> [ListEncoding a]
encodeDirect [] = []
encodeDirect [x] = [Single x]
encodeDirect (x1:x2:xs)
    | x1 == x2  = incrementFirst resultTail
    | otherwise = (Single x1):resultTail
    where resultTail = encodeDirect (x2:xs)

incrementFirst :: [ListEncoding a] -> [ListEncoding a]
incrementFirst ((Single x):rest) = (Multiple 2 x):rest
incrementFirst ((Multiple c x):rest) = (Multiple (c + 1) x):rest