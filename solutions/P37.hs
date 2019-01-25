import Data.List

-- Calculate Euler's totient function phi(m) (improved).
totient' :: Integer -> Integer
totient' n = foldl compute 1 (primeFactorsMult n)
    where compute acc (p, m) = acc * (p - 1) * p ^ (m - 1)

-- Calculate Euler's totient function phi(m) (using product instead of foldl).
totient'' :: Integer -> Integer
totient'' = product . (map compute) . primeFactorsMult
    where compute (p, m) = (p - 1) * p ^ (m - 1)

-- Helper function from P36
primeFactorsMult :: Integer -> [(Integer, Int)]
primeFactorsMult n = map count (group factors)
    where factors = primeFactorsHelp 2 (abs n)
          count xs = (head xs, length xs)

-- Helper function to generate prime factors
primeFactorsHelp :: Integer -> Integer -> [Integer]
primeFactorsHelp _ 1 = []
primeFactorsHelp d n
    | n `mod` d == 0 = d : primeFactorsHelp d (n `div` d)
    | otherwise      = primeFactorsHelp (d + 1) n