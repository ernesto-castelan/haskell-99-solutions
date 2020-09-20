import Data.List

-- Determine the prime factors of a given positive integer.
-- Construct a list containing the prime factors and their multiplicity.
primeFactorsMult :: Integer -> [(Integer, Int)]
primeFactorsMult n = map count (group factors)
    where factors = primeFactorsHelp 2 (abs n)
          count xs = (head xs, length xs)

-- Helper function to generate prime factors
primeFactorsHelp :: Integer -> Integer -> [Integer]
primeFactorsHelp _ 1 = []
primeFactorsHelp d n
    | d `divides` n = d : primeFactorsHelp d (n `div` d)
    | otherwise     = primeFactorsHelp (d + 1) n

-- Helper function to check if a number is divisible by other
divides :: Integer -> Integer -> Bool
divides b a = a `mod` b == 0