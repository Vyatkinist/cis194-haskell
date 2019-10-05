{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE ViewPatterns #-}

module Golf where

skips :: [a] -> [[a]]
skips xs = map (\n -> everyNth n xs) [1..(length xs)]
-- First, create pairs of (element, index), 
-- filter them by even index and at last,
-- use map to extract element out of tuple
    where everyNth n xs = map (\(el, index) -> el) $ filter (\(el, index) -> index == n) $ zip xs $ cycle [1..n]

main :: IO()
main = do
    putStrLn . show $ skips "ABCD"       == ["ABCD", "BD", "C", "D"]
    putStrLn . show $ skips "hello!"     == ["hello!", "el!", "l!", "l", "o", "!"]
    putStrLn . show $ skips [1]          == [[1]]
    putStrLn . show $ skips [True,False] == [[True,False], [False]]
