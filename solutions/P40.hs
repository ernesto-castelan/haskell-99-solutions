import Data.List

-- Write a predicate to find the two prime numbers that sum up to a given even integer (monad binding)
goldbach :: Integer -> Maybe (Integer, Integer)
goldbach n = findFirst n >>= makePair n

-- Write a predicate to find the two prime numbers that sum up to a given even integer (do notation)
goldbach' :: Integer -> Maybe (Integer, Integer)
goldbach' n = do
    a <- findFirst n
    makePair n a

-- Helper function. Given the first number, returns the complete of the Goldbach's decomposition
makePair :: Integer -> Integer -> Maybe (Integer, Integer)
makePair n a = Just (a, n - a)

-- Helper function. Returns the first number of the Goldbach's decomposition
findFirst :: Integer -> Maybe Integer
findFirst n = find (\x -> isPrime x && isPrime (n - x)) [1..n]

-- Helper function from P31 (modified)
isPrime :: Integer -> Bool
isPrime n
    | n <= 1    = False
    | otherwise = not $ any ((==) 0 . mod n) [2..(n - 1)]