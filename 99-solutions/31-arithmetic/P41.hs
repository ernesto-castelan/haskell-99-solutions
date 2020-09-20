import Data.Maybe
import Data.List

-- Given a range of integers by its lower and upper limit,
-- print a list of all even numbers and their Goldbach composition.
goldbachList :: Integer -> Integer -> [(Integer, Integer)]
goldbachList a b = map fromJust (map goldbach (filter even [a..b]))

-- Find out how many cases are there such that the primes are both bigger than C.
goldbachList' :: Integer -> Integer -> Integer -> [(Integer, Integer)]
goldbachList' a b c = [(x, y) | (x, y) <- goldbachList a b, x > c, y > c]

-- Helper function from P40
goldbach :: Integer -> Maybe (Integer, Integer)
goldbach n = findFirst n >>= makePair n

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