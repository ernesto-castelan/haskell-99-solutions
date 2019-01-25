-- Split a list into two parts; the length of the first part is given.
split :: [a] -> Int -> ([a], [a])
split xs n = (takeFirsts xs n, dropFirsts xs n)

-- Takes the first n elements
takeFirsts :: [a] -> Int -> [a]
takeFirsts [] _ = []
takeFirsts _ n | n <= 0 = []
takeFirsts (x:xs) n = x:(takeFirsts xs (n - 1))

-- Drops the first n elements
dropFirsts :: [a] -> Int -> [a]
dropFirsts [] _ = []
dropFirsts xs 0 = xs
dropFirsts (x:xs) n = dropFirsts xs (n - 1)

-- Split a list into two parts; the length of the first part is given (not using helper functions)
split' :: [a] -> Int -> ([a], [a])
split' [] _ = ([], [])
split' (x:xs) n
    | n > 0     = let (l, r) = split' xs (n - 1) in (x:l, r)
    | otherwise = ([], x:xs)