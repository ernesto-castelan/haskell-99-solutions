-- Consecutive duplicates of elements are encoded as tuples (N E) where N is the number of duplicates of the element E.
encode :: (Eq a) => [a] -> [(Int, a)]
encode = map ( \elem -> (length elem, head elem) ) . pack'

-- Use the result of problem P09 to implement the so-called run-length encoding data compression method.
-- Pack consecutive duplicates of list elements into sublists.
pack' :: (Eq a) => [a] -> [[a]]
pack' [] = []
pack' all@(x:xs) = chunk : (pack' rest)
    where (chunk, rest) = span (== x) all

-- Consecutive duplicates of elements are encoded as tuples (no use of helper function)
encode' :: (Eq a) => [a] -> [(Int, a)]
encode' [] = []
encode' all@(x:xs) = (length chunk, x) : (encode' rest)
    where (chunk, rest) = span (== x) all