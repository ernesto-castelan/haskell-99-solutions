-- Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.
primes :: Integer -> Integer -> [Integer]
primes a b = filter isPrime [a..b]

-- Helper function from P31 (modified)
isPrime :: Integer -> Bool
isPrime n
    | n <= 1    = False
    | otherwise = not $ any ((==) 0 . mod n) [2..(n - 1)]