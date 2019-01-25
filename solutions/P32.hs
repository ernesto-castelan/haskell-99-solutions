-- Determine the greatest common divisor of two positive integer numbers. Use Euclid's algorithm.
myGcd :: Integer -> Integer -> Integer
myGcd a 0 = abs a
myGcd a b = myGcd b (a `mod` b)