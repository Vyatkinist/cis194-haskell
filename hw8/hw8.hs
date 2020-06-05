{-# LANGUAGE FlexibleInstances #-}

module HW8 where

import Data.Monoid
import Data.Tree
import Employee

-- Exercise 1




glCons :: Employee -> GuestList -> GuestList
glCons e@Emp { empFun = eFun } (GL list fun) = GL (e:list) (fun + eFun)

instance Semigroup GuestList where
    (<>) (GL l1 f1) (GL l2 f2) = GL (l1 ++ l2) (f1 + f2)

instance Monoid GuestList where
    mempty = GL [] 0

moreFun :: GuestList -> GuestList -> GuestList
moreFun l1 l2 = if l1 >= l2 then l1 else l2

-- Exercise 2

treeDepth :: Tree a -> Integer
treeDepth Node { subForest = [] } = 1
treeDepth Node { rootLabel = root, subForest = forest } = 1 + maximum (map treeDepth forest)

treeSum :: Tree Integer -> Integer
treeSum (Node root []) = root
treeSum (Node root forest) = root + sum (map treeSum forest)

treeSize :: Tree a -> Integer
treeSize (Node root []) = 1
treeSize (Node root forest) = 1 + sum (map treeSize forest)

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f Node { rootLabel = root, subForest = forest } = f root (map (\tree -> treeFold f tree) forest)

treeSum' :: Tree Integer -> Integer
treeSum' tree@(Node root forest) = treeFold (\root forest -> root + sum forest) tree
    -- where 
    --       foldFun :: Integer -> [Integer] -> Integer
    --       foldFun root forest = root + sum forest
        --   sumForest :: [Tree Integer] -> Integer
        --   sumForest forest = sum $ map treeSum' forest


testTree = Node 2 [(Node 4 []), (Node 5 [(Node 6 [])])]

employee = Emp { empName = "Matthew", empFun = 10 }
empList = GL [Emp "Joe" 5, Emp "John" 15] 20
empList1 = GL [Emp "Norman" 200, Emp "Polly" 100] 300

main :: IO()
--main = putStrLn "Test" >> (readLn >>= \s -> putStrLn s)
main = do
    print $ glCons employee empList
    print $ empList <> empList1
    print $ empList `moreFun` empList1
    print $ treeDepth testTree
    print $ treeSum testTree
    print $ treeSize testTree
    print $ treeSum' testTree