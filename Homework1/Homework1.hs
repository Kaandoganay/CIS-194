--Exercise 1--
toDigits :: Integer -> [Integer]
toDigits x  | x > 0  = toDigits (x`div` 10) ++ [x `mod`10]
            | otherwise = []

toDigitsRev :: Integer -> [Integer]
toDigitsRev x = reverse (toDigits x)


--Exercise2--
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x*2]
doubleEveryOther [x, y] =[x*2, y]
doubleEveryOther xs= doubleEveryOther (init (init xs)) ++ [last (init xs)*2] ++[last xs]

--Exercise3--
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [x] = x `div` 10 + x `mod` 10
sumDigits (x:xs) = x `div` 10 + x `mod` 10 + sumDigits xs

--Exercise4--
validate :: Integer -> Bool
validate x | sumDigits(doubleEveryOther(toDigits x)) `mod` 10 == 0 = True 
           | otherwise = False

--Exercise5--
