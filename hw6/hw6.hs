--{-# OPTIONS_GHC -Wall -Werror #-}



{-# LANGUAGE FlexibleInstances #-}

module Calc where

import Prelude


-- Exercise 1

fib :: Integer -> Integer
fib n
    | n == 0    = 0
    | n == 1    = 1
    | n > 1     = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer] -> [Integer]
fibs1 = map fib

-- Exercise 2

getTwoFirstElements :: [Integer] -> [Integer]
getTwoFirstElements [] = [0, 0]
getTwoFirstElements [x] = [x, 0]
getTwoFirstElements (x:y:xs) = [x, y]

getTwoLastElements :: [Integer] -> [Integer]
getTwoLastElements = getTwoFirstElements . reverse

sumTwoLast :: [Integer] -> Integer
sumTwoLast xs = sum $ getTwoLastElements xs

fibsWithState :: Integer -> [Integer] -> Integer
fibsWithState 0 _ = 0
fibsWithState 1 _ = 1
fibsWithState _ xs = sumTwoLast xs

fibs2 :: [Integer] -> [Integer]
fibs2 = foldl (\acc x -> acc ++ [fibsWithState x acc]) []

fib0 :: Integer -> Integer -> [Integer]
fib0 x y = x : fib0 y (x+y)

fibs3 :: [Integer]
fibs3 = fib0 0 1

-- Exercise 3

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

instance Show a => Show (Stream a) where
    show = show . take 20 . streamToList

-- Exercise 4

streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))

-- Exercise 5

streamTail :: Stream a -> Stream a
streamTail (Cons x xs) = xs

nats :: Stream Integer
nats = streamFromSeed (+1) 0

largestPowOfTwoThatEvenlyDividesN :: Integer -> Integer
largestPowOfTwoThatEvenlyDividesN n
    | even n    = floor . logBase 2 . fromIntegral $ n
    | otherwise = 0

ruler :: Stream Integer
ruler = streamMap largestPowOfTwoThatEvenlyDividesN (streamTail nats)

main :: IO()
main = do
    --print $ map fib [0..10]


    -- print $ getTwoLastElements []
    -- print $ getTwoLastElements [2]
    -- print $ sumTwoLast [1..10]
    -- print $ sumTwoLast [1, 2, 3]
    -- print $ fibsWithState 5 [0, 1, 1, 2, 3]

    print $ take 30 fibs3
    print $ take 20 (streamToList nats)
    print ruler