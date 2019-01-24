-- We have to define a new data type, because lists in Haskell are homogeneous.
data NestedList a = Elem a | List [NestedList a]

-- Flatten a nested list structure.
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x:xs)) = (flatten x) ++ (flatten $ List xs)