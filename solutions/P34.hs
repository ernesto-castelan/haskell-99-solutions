-- Calculate Euler's totient function phi(m).
-- Euler's so-called totient function phi(m) is defined as the number of positive integers r (1 <= r < m) that are coprime to m.
totient :: Integer -> Int
totient m = length $ filter (coprime m) [1..(m - 1)]

-- Helper function from P33
coprime :: Integer -> Integer -> Bool
coprime a b = myGcd a b == 1

-- Helper function from P32
myGcd :: Integer -> Integer -> Integer
myGcd a 0 = abs a
myGcd a b = myGcd b (a `mod` b)