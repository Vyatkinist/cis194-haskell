module Hw4 where

import Prelude

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x    = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
    | even n    = n + fun2 (n `div` 2)
    | otherwise = fun2 (3*n + 1)

fun1' :: [Integer] -> Integer
fun1' xs = product $ map (\x -> x - 2) $ filter even xs

fun2' :: Integer -> Integer
-- map every number to even
fun2' n = sum . filter even . takeWhile (> 1) $ iterate (\x -> if even x then (div x 2) else (3*x + 1)) n

data Tree a = Leaf | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

--foldTree :: [a] -> Tree a
--foldTree x = sort x

xor :: [Bool] -> Bool
xor x = odd $ foldl countTrue 0 x
    where
        countTrue acc x = if x then acc + 1 else acc

map' :: (a -> b) -> [a] -> [b]
map' f x = foldr mapFold [] x
    where 
        mapFold x acc = (f x) : acc


anotherFoldl :: (a -> b -> a) -> a -> [b] -> a
anotherFoldl f acc (x:xs) = (f acc x)

sumArr :: [Integer] -> Integer
sumArr (x:xs) = (+) x (sumArr xs)
sumArr [] = 0

foldArr :: (Integer -> Integer -> Integer) -> Integer -> [Integer] -> Integer
foldArr f acc [] = acc
foldArr f acc (x:xs) = f x (foldArr f acc xs)

filterr :: (a -> Bool) -> [a] -> [a]
filterr _ [] = []
filterr f (x: xs)
    | f x   = x : filterr f xs
    | otherwise = filterr f xs

fold'' :: (a -> b -> a) -> a -> [b] -> a
fold'' f acc [] = acc
fold'' f acc (x:xs) = f (fold'' f acc xs) x

map''' :: (a -> b) -> [a] -> [b]
map''' f xs = foldr foldMap [] xs
        where foldMap x xs = f x : xs

xor''' :: [Bool] -> Bool
xor''' xs = odd $ foldr countTrue' 0 xs
        where countTrue' x acc = if x then acc + 1 else acc

reverse' :: [a] -> [a]
reverse' xs = foldr rev [] xs
        where rev acc xs = xs ++ [acc]

main :: IO()
main = do
    putStrLn . show $ zip [1..10] $ map fun2 [1..10]
    putStrLn . show $ zip [1..10] $ map fun2' [1..10]
    
    putStrLn . show $ xor [False, True, False] == True
    putStrLn . show $ xor [False, True, False, False, True] == False
    putStrLn . show $ xor [False, True, True]
    putStrLn . show $ xor''' [False, True, False] == True
    putStrLn . show $ xor''' [False, True, False, False, True] == False
    putStrLn . show $ xor''' [False, True, True]    
    -- putStrLn . show $ foldTree [4,3,2,6,2,7]
    -- putStrLn . show $ foldTree [1..5]
    putStrLn . show $ anotherFoldl (+) 0 [1..4]
    --printShow $ mapp (\x -> x + 1) [1..3]
    putStrLn . show $ filterr even [1..3]
    putStrLn . show $ foldArr (*) 1 [1..4]
    putStrLn . show $ fold'' (+) 0 [1..4]
    putStrLn . show $ reverse' [1..4]