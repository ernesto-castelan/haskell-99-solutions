-- Find the last element of a list.
myLast :: [a] -> a
myLast [] = error "Does not work on empty lists"
myLast [x] = x
myLast (x:xs) = myLast xs