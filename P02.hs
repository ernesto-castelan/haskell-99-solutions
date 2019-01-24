-- Find the last but one element of a list.
myButLast :: [a] -> a
myButLast [] = error "Does not work on empty lists"
myButLast [x] = error "Does not work on singleton lists"
myButLast [x, y] = x
myButLast (x:xs) = myButLast xs