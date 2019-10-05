{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE ViewPatterns #-}

module Golf where

skips :: [a] -> [[a]]
skips xs = map (\n -> everyNth n xs) [1..(length xs)]
-- First, create pairs of (element, index), 
-- filter them by even index and at last,
-- use map to extract element out of tuple
    where everyNth n xs = map (\(el, index) -> el) $ filter (\(el, index) -> index == n) $ zip xs $ cycle [1..n]

localMaxima :: [Integer] -> [Integer]
localMaxima xs = map (\(a, b, c) -> b) $ filter (\(a, b, c) -> b > c && b > a) $ zip3 xs (drop 1 xs) (drop 2 xs)

main :: IO()
main = do
    putStrLn "Exercise 1:"
    putStrLn . show $ skips "ABCD"       == ["ABCD", "BD", "C", "D"]
    putStrLn . show $ skips "hello!"     == ["hello!", "el!", "l!", "l", "o", "!"]
    putStrLn . show $ skips [1]          == [[1]]
    putStrLn . show $ skips [True,False] == [[True,False], [False]]

    putStrLn "Exercise 2:"
    putStrLn . show $ localMaxima [2,9,5,6,1] == [9,6]
    putStrLn . show $ localMaxima [2,3,4,1,5] == [4]
    putStrLn . show $ localMaxima [1,2,3,4,5] == []
    