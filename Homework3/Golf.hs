module Golf where
--Exercise 1 Hopscotch--
taken :: Int -> [a] -> [a]
taken n xs = [xs !! k | k <- [n-1, n-1+n..length xs - 1]]

skips :: [a] -> [[a]]
skips xs = [taken n xs | n <- [1..length xs ]  ]

--exercise2--

localMaxima xs = [n | n <-xs,   ]