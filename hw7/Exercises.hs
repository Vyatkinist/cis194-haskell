{-# LANGUAGE FlexibleInstances #-}
module Exercises where

import JoinList
import Sized
import Scrabble
import Buffer

-- Exercise 1

tag :: Monoid m => JoinList m a -> m
tag (Append m _ _) = m
tag (Single m _) = m
tag Empty = mempty

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) a b = Append (tag a <> tag b) a b

-- Exercise 2

a :: JoinList Size String
a = Append (Size 4)
        (Append (Size 3)
            (Single (Size 1) "y")
            (Append (Size 2)
                (Single (Size 1) "e")
                (Single (Size 1) "a")))
        (Single (Size 1) "h")

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ a b) = jlToList a ++ jlToList b

(!!?) :: [a] -> Int -> Maybe a
(!!?) [] _          = Nothing
(!!?) _ i
    | i < 0         = Nothing
(!!?) (x:xs) 0      = Just x
(!!?) (x:xs) i      = xs !!? (i - 1)

getNodeSize :: (Sized m, Monoid m) => JoinList m a -> Int
getNodeSize = getSize . size . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i (Single _ value)
    | i == 0    = Just value
    | otherwise = Nothing
indexJ i (Append size left right)
    | i < 0 || i >= lSize + rSize       = Nothing
    | i < lSize                         = indexJ i left
    | i >= lSize && i < (lSize + rSize) = indexJ (i - lSize) right
    where lSize = getNodeSize left
          rSize = getNodeSize right

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ _ leaf@(Single _ _) = leaf
dropJ n root@(Append tSize left right)
    | n >= totalSize    = Empty
    | n <= 0            = root
    | n >= lSize        = dropJ (n - lSize) right
    | n < lSize         = dropJ n left +++ right
    where lSize = getNodeSize left
          rSize = getNodeSize right
          totalSize = getSize . size $ tSize

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ 0 _ = Empty 
takeJ n leaf@(Single _ _) = leaf
takeJ n root@(Append tSize left right)
    | n >= totalSize    = root
    | n <= 0            = Empty 
    | n > lSize         = left +++ takeJ (n - lSize) right
    | n <= lSize        = takeJ n left
    where lSize = getNodeSize left
          rSize = getNodeSize right
          totalSize = getSize . size $ tSize

checkIndexes :: (Sized m, Monoid m) => JoinList m a -> [Int] -> [Maybe a]
checkIndexes list = map (`indexJ` list)

-- Exercise 3

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

testScores = scoreLine "yay " +++ scoreLine "haskell!"

-- Exercise 4

instance Buffer (JoinList (Score, Size) String) where
    toString = unlines . jlToList
    fromString s = foldl (\acc x -> acc +++ Single (scoreString x, Size 1) x) Empty (lines s)
    line = indexJ
    replaceLine index line buffer = takeJ index buffer +++ fromString line +++ dropJ (index + 1) buffer
    numLines = getSize . snd . tag
    value = getScore . fst . tag

-- main :: IO()
-- main = do
--     putStrLn "HW7:"
--     -- print a
--     -- print . unwords . jlToList $ a
--     -- print $ checkIndexes a [-1..5]
--     -- print $ map (`dropJ` a) [0..5]
--     print $ takeJ (-4) a
--     print testScores