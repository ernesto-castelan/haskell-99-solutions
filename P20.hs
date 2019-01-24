-- Remove the K'th element from a list.
removeAt :: Int -> [a] -> (a, [a])
removeAt _ [] = error "Does not work on empty lists"
removeAt 1 (x:xs) = (x, xs)
removeAt k (x:xs) = (l, x:r) where (l, r) = removeAt (k - 1) xs