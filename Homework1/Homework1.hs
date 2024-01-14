--Exercise 1--
toDigits :: Integer -> [Integer]
toDigits x  | x > 0  = toDigits (x`div` 10) ++ [x `mod`10]
            | otherwise = [] 

toDigitsRev :: Integer -> [Integer]
toDigitsRev x = reverse (toDigits x)


