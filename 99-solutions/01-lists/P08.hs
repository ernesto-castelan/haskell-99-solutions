-- Eliminate consecutive duplicates of list elements.
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:xs)
    | x == next = compress xs
    | otherwise = x : compress xs
    where next = head xs