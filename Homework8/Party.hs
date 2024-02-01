{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-noncanonical-monoid-instances #-}

module Party where
import Data.List
import Data.Monoid
import Data.Tree
import Employee

--exercise1--

glCons :: Employee -> GuestList -> GuestList
glCons x (GL xs a) = GL (x:xs) (a + empFun x)


instance Monoid GuestList where
    mempty                      = GL [] 0

instance Semigroup GuestList where
    (GL xs a) <> (GL ys b) = GL (xs++ys) (a+b)

moreFun :: GuestList -> GuestList -> GuestList
moreFun  = max

--exercise2--

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node a sf) = f a (treeFold f <$> sf)

--exercise3--
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss bestLst = ( withBoss, withoutBoss)
  where withoutBoss   = foldMap fst bestLst
        withBoss      = glCons boss withoutSubBoss
        withoutSubBoss = foldMap snd bestLst

--exercise4--

maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel

--Exercise5--

newGL :: GuestList -> String
newGL (GL xs a) = "Total fun: " ++ show a ++ "\n" ++ unlines employees
  where employees = map (\(Emp {empName = name}) -> name) xs

main :: IO()
main = do
  list <- readFile "company.txt"
  putStr . newGL . maxFun . read $ list