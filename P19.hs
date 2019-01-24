-- Rotate a list N places to the left.
rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate xs 0 = xs
rotate xs n | n < 0 = rotate xs (n + length xs)
rotate (x:xs) n = rotate (xs ++ [x]) (n - 1)