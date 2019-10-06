{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE ViewPatterns #-}

module Golf where
import Data.Tuple
import Data.List

skips :: [a] -> [[a]]
skips xs = map (\n -> everyNth n xs) [1..(length xs)]
-- First, create pairs of (element, index), 
-- filter them by even index and at last,
-- use map to extract element out of tuple
    where everyNth n xs = map (\(el, index) -> el) $ filter (\(el, index) -> index == n) $ zip xs $ cycle [1..n]

localMaxima :: [Integer] -> [Integer]
-- Create triplets with offset 1 from array, filter ones with middle element to be the largest
localMaxima xs = map (\(a, b, c) -> b) $ filter (\(a, b, c) -> b > c && b > a) $ zip3 xs (drop 1 xs) (drop 2 xs)

histogram :: [Integer] -> String
histogram xs = reverse . unlines $ legend ++ map (\row -> histString row $ pairs xs) [1..maxRow]
    where
        -- Create an array of '=0', '=1' etc, and then transpose
        legend = transpose . reverse $ map (\n -> "=" ++ show n) [0..9]
        -- Gets the maximum column height
        maxRow = maximum $ snd <$> pairs xs
        -- Make a list of 0..9, get counts for every number
        pairs xs = map (\n -> (n, countTimes n xs)) [0..9]
            where 
                -- Count how many times n occurs in a list
                countTimes n xs = sum $ map (\x -> 1) $ filter (\x -> x == n) xs
        -- Get a single row of a histogram        
        histString row xs = reverse $ map (\(index, count) -> numToChar row count) xs
            where
                numToChar row c
                    | c < row   = ' '
                    | c >= row  = '*'

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

    putStrLn "Exercise 3:"
    putStrLn $ histogram [1,4,5,4,6,6,3,4,2,4,9]
    putStrLn $ histogram [1,1,1,5]
