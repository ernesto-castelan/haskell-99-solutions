-- We have to define a new data type, because lists in Haskell are homogeneous.
data ListEncoding a = Single a | Multiple Int a deriving (Show)

decodeModified :: [ListEncoding a] -> [a]
decodeModified = concatMap unpack

-- Transforms ListEncoding into list
unpack :: ListEncoding a -> [a]
unpack (Single x) = [x]
unpack (Multiple n x) = replicate n x