-- Determine whether a given integer number is prime.
isPrime :: Integer -> Bool
isPrime n
    | n <= 1    = False
    | otherwise = not $ any (divides n) [2..(n - 1)]

-- Helper function to check if a number is divisible by other
divides :: Integer -> Integer -> Bool
divides a b = a `mod` b == 0