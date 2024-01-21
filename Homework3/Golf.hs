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

--exercise3--

histogram :: [Integer] -> String
histogram xs = unlines (map (stars a) [k+1,k..1]) ++ "==========\n0123456789\n"
     where a = count xs
           k = maximum a

stars :: [Int] -> Int -> String
stars xs n = [if i >= n then '*' else ' ' | i <- xs]

count :: [Integer] -> [Int]
count xs = map (\n -> length $ filter (== n) xs) [0..9]
