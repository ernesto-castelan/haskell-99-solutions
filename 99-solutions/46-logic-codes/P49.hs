-- Write a function that generates an n-bit Gray code
gray :: Integer -> [String]
gray 0 = [""]
gray n = map ('0':) partial ++ map ('1':) (reverse partial)
    where partial = gray $ n - 1