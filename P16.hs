-- Drop every N'th element from a list.
dropEvery :: [a] -> Int -> [a]
dropEvery xs n = dropHelper n n xs

-- Helper function
dropHelper ::  Int -> Int -> [a] -> [a]
dropHelper _ _ [] = []
dropHelper n 1 (x:xs) = dropHelper n n xs
dropHelper n c (x:xs) = x : (dropHelper n (c - 1) xs)