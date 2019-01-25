-- We have to define a new data type, because lists in Haskell are homogeneous.
data ListEncoding a = Single a | Multiple Int a deriving (Show)

-- Modified run-length encoding.
encodeModified :: (Eq a) => [a] -> [ListEncoding a]
encodeModified [] = []
encodeModified all@(x:xs) = (pack chunk) : (encodeModified rest)
    where (chunk, rest) = span (== x) all

-- Transforms list into ListEncoding
pack :: [a] -> ListEncoding a
pack [] = error "Does not work on empty lists"
pack [x] = Single x
pack all@(x:xs) = Multiple (length all) x