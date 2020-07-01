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

treeSize' :: Tree a -> Integer
treeSize' tree@(Node root forest) = treeFold (\root forest -> 1 + sum forest) tree

treeDepth' :: Tree a -> Integer
treeDepth' tree@(Node root forest) = treeFold (\root forest -> 1 + maximum (0:forest)) tree

testTree = Node 2 [(Node 4 []), (Node 5 [(Node 6 [])])]

employee = Emp { empName = "Matthew", empFun = 10 }
empList = GL [Emp "Joe" 5, Emp "John" 15] 20
empList1 = GL [Emp "Norman" 200, Emp "Polly" 100] 300

nextLevel' :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel' boss results = (withBoss, withoutBoss)
  where
      -- with boss, get first list of guestlists, append boss to each, find maximum
      withBoss = maximumS ( map (\list -> glCons boss list) (map fst results ) )
      -- without boss, get second list of guestlists, find maximum
      withoutBoss = maximumS ( map snd results )

-- nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
-- nextLevel boss results = (withBoss, withoutBoss)
--   where
--     withoutBoss = foldMap (uncurry moreFun) results
--     withBoss = glCons boss $ foldMap snd results

-- | First part of list is with boss.
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss bestLists = (maximumS withBossL, maximumS withoutBossL)
  where withoutBossL   = map fst bestLists
        -- ^ The new withoutBossList has sub bosses in it.

        withoutSubBoss = map snd bestLists
        withBossL      = map (glCons boss) withoutSubBoss
        -- ^ The new withBossList doesn't have sub bosses in it.

maximumS :: (Monoid a, Ord a) => [a] -> a
maximumS [] = mempty
maximumS lst = maximum lst        

--maxFun :: Tree Employee -> GuestList
--maxFun tree = uncurry moreFun . nextLevel tree

maxFun :: Tree Employee -> GuestList
maxFun t = uncurry moreFun $ (treeFold nextLevel t)

-- Convert tree into a list
test :: Tree Employee -> (GuestList, GuestList)
test a = treeFold (\node -> nextLevel' node ) a 

boss = Emp "Joe" 5
guestLists = [(GL [Emp "Stan" 9] 9, GL [Emp "Bob" 3] 3)]

main :: IO()
--main = putStrLn "Test" >> (readLn >>= \s -> putStrLn s)
main = do
    -- print $ glCons employee empList
    -- print $ empList <> empList1
    -- print $ empList `moreFun` empList1
    -- print $ treeSum testTree
    -- print $ treeSum' testTree
    -- print $ treeSize testTree
    -- print $ treeSize' testTree
    -- print $ treeDepth testTree
    -- print $ treeDepth' testTree
    print . fst $ nextLevel boss guestLists
    print . snd $ nextLevel boss guestLists
    print $ (nextLevel employee [(empList, empList)])
    print $ (nextLevel' employee [(empList, empList)])
    --print . test $ testCompany 
    --print $ maximum $ (map snd [(empList, empList1)] )
    print $ maxFun testCompany2 