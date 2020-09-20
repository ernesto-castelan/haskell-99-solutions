-- Find the K'th element of a list. The first element in the list is number 1.
elementAt :: [a] -> Integer -> a
elementAt _ k | k < 1 = error "Index out of bounds"
elementAt [] _ = error "Index out of bounds"
elementAt (x:xs) 1 = x
elementAt (x:xs) k = elementAt xs (k-1)