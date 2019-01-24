-- Find the number of elements of a list.
myLength :: [a] -> Integer
myLength [] = 0
myLength (x:xs) = myLength xs + 1