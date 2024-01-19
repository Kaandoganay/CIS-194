module Golf where

--Exercise 1 Hopscotch--
taken :: Int -> [a] -> [a]
taken n xs = [xs !! k | k <- [n-1, n-1+n..length xs - 1]]

skips :: [a] -> [[a]]
skips xs = [taken n xs | n <- [1..length xs ]  ]

--exercise2--
localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:xs)
  | x < y && y > z = y : localMaxima (y:z:xs)
  | otherwise      = localMaxima (y:z:xs)
localMaxima _ = []