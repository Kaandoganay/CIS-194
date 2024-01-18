module Golf where
--Exercise 1 Hopscotch--

skips :: [a] -> [[a]]
skips xs= map (drop 3) (replicate (length xs) xs) 