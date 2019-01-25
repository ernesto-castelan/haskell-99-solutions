-- Reverse a list (slow)
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- Reverse a list (fast, using bulit in functions)
myReverse' :: [a] -> [a]
myReverse' = foldl (flip (:)) []