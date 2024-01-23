--Exercise1--
--fun1 :: [Integer] -> Integer
--fun1 [] = 1
--fun1 (x:xs)
-- | even x = (x - 2) * fun1 xs
-- | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1'= product . map (\x -> x-2) . filter even

--fun2 :: Integer -> Integer
--fun2 1 = 0
--fun2 n | even n = n + fun2 (n ‘div‘ 2)
-- | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2'= sum . takeWhile (/=1) . iterate (\x -> if even x then x `div` 2 else 3*x+1)

--Exercise2--

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)



height :: Tree a -> Integer
height (Node x _ _ _) = x
height Leaf       = 0

balance :: a -> Tree a -> Tree a
balance x Leaf = Node 0 Leaf x Leaf
balance x (Node h left r right)
  | h1 < h2   = Node h (balance x left) r right
  | h1 > h2   = Node h left r (balance x right)
  | otherwise = Node (h3 + 1) (balance x left) r right
  where h1 = height left
        h2 = height right
        h3 = height (balance x left)
        
foldTree :: [a] -> Tree a
foldTree = foldr balance Leaf

--Exercise3--
xor :: [Bool] -> Bool
xor = foldr (/=) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

--Exercise4--
cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

