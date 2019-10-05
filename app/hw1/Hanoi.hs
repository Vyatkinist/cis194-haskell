{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE ViewPatterns #-}

module Hanoi where

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg-> Peg -> Peg -> [Move]
hanoi n from to spare
    | n == 1 =      [(from, to)]
    | otherwise =   (hanoi (n-1) from spare to) ++ (hanoi 1 from to spare) ++ (hanoi (n-1) spare to from)

solved::Bool
solved = hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")]

main:: IO ()
main = do
    putStrLn (show solved)
