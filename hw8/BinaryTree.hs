module BinaryTrees where

-- Folds on binary trees
data Tree a = Node (Tree a) a (Tree a) | Empty
  deriving (Show, Eq)

treeSize :: Tree a -> Integer
treeSize Empty = 0
treeSize (Node l _ r) = treeSize l + 1 + treeSize r

treeSum :: Tree Integer -> Integer
treeSum Empty = 0
treeSum (Node l x r) = x + treeSum l + treeSum r

treeDepth :: Tree a -> Integer
treeDepth Empty = 0
treeDepth (Node l _ r) = 1 + max (treeDepth l) (treeDepth r)

treeFold :: b -> (b -> a -> b -> b) -> Tree a -> b
treeFold e f Empty = e
treeFold e f (Node l x r) = f (treeFold e f l) x (treeFold e f r)

a = Node (Node Empty 4 Empty) 6 (Node Empty 1 (Node Empty 7 Empty))

main :: IO()
main = do
    putStrLn "I have no idea how to implement it."
    print . treeSize $ a
    print . treeSum $ a
    print $ treeFold 0 (\l x r -> l + x + r) a
    print $ treeFold 0 (\l _ r -> l + 1 + r) a
    print $ treeDepth a
