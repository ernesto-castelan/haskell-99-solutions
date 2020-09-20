-- Determine the prime factors of a given positive integer.
-- Construct a flat list containing the prime factors in ascending order.
primeFactors :: Integer -> [Integer]
primeFactors n = primeFactorsHelp 2 (abs n)

-- Helper function to generate prime factors
primeFactorsHelp :: Integer -> Integer -> [Integer]
primeFactorsHelp _ 1 = []
primeFactorsHelp d n
    | d `divides` n = d : primeFactorsHelp d (n `div` d)
    | otherwise     = primeFactorsHelp (d + 1) n

-- Helper function to check if a number is divisible by other
divides :: Integer -> Integer -> Bool
divides b a = a `mod` b == 0