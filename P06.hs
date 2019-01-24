-- Find out whether a list is a palindrome.
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == myReverse' xs

-- Helper function from P05
myReverse' :: [a] -> [a]
myReverse' = foldl (flip (:)) []

-- Find out whether a list is a palindrome (with no helper function)
isPalindrome' :: (Eq a) => [a] -> Bool
isPalindrome' [] = True
isPalindrome' [x] = True
isPalindrome' list
    | a == z    = isPalindrome . tail . init $ list
    | otherwise = False
    where
        a = head list
        z = last list