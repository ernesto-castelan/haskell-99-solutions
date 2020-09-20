-- Extract a slice from a list (start counting the elements with 1, both limits included)
slice :: [a] -> Int -> Int -> [a]
slice xs i k = slice' xs (i - 1) k

-- Extract a slice from a list (zero index version, upper limit excluded)
slice' :: [a] -> Int -> Int -> [a]
slice' [] _ _ = []
slice' _ 0 0 = []
slice' (x:xs) 0 k = x : (slice' xs 0 (k - 1))
slice' (x:xs) i k = slice' xs (i - 1) (k - 1)