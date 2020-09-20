-- Determine whether two positive integer numbers are coprime.
-- Two numbers are coprime if their greatest common divisor equals 1.
coprime :: Integer -> Integer -> Bool
coprime a b = myGcd a b == 1

-- Helper function from P32
myGcd :: Integer -> Integer -> Integer
myGcd a 0 = abs a
myGcd a b = myGcd b (a `mod` b)