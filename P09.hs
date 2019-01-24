import Data.List

-- Pack consecutive duplicates of list elements into sublists.
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack (x:xs)
    | x == next = (x : head rest) : (tail rest)
    | otherwise = [x] : rest
    where next = head xs
          rest = pack xs

-- Pack consecutive duplicates of list elements into sublists (using list functions)
pack' :: (Eq a) => [a] -> [[a]]
pack' [] = []
-- pack' [x] = [[x]]  -- It's unnecesary
pack' all@(x:xs) = chunk : (pack' rest)
    where (chunk, rest) = span (== x) all